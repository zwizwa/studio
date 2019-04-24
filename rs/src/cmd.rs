// Templated from exo/rs/src/cmd.rs

// This module bridges {packet,4} ETF and Rust code, while providing a
// cache for object storage.

#![feature(rustc_private)] 

extern crate erl_rs;
extern crate eetf;
extern crate itertools;
extern crate jack;
//extern crate libpulse_simple_binding;
//extern crate libpulse_binding;

use eetf::{Term,Atom};
use std::process;

mod etflog;
//mod timeseries;
mod samplestore;
mod jack_client;
//mod pulse;


// use self::erl_rs::{as_vec, as_i32, as_str, as_u8_slice, atom, tag, binary, i32, i32_vec};
// constructors
use self::erl_rs::{atom, tag, i32, i32_vec, i32_vec2, binary};
// deconstructors
use self::erl_rs::{as_str, as_vec, as_i32};

/* Packet handler */
fn main() {
    eprintln!("exo_rs: Rust {{packet,4}} port.");
    let mut ctx = Context { 
        count: 0,
        cache: Vec::new()
    };
    let _rv = erl_rs::loop_apply_etf_2(
        &mut |cmd| dispatch(&mut ctx, cmd),
        &mut std::io::stdin(),
        &mut std::io::stdout());
}

/* Context manages state that is persistent across calls.  Note that
   this should only contain caches, e.g. the open spec fully
   determines the object, such that the Erlang side can re-open
   resoureces on restart. */

pub enum Object {
    Empty,
    EtfLog      { obj: Box<etflog::EtfLog> },
    SampleStore { obj: Box<samplestore::SampleStore> },
    JackClient  { obj: Box<jack_client::JackClient /* Trait object */> }
} 
pub struct Context {
    count: u32,
    cache: Vec<Object>
}
fn cache_insert<'a>(ctx: &'a mut Context, obj: Object) -> usize {
    let l = ctx.cache.len();
    for i in 0..l {
        match ctx.cache[i] {
            Object::Empty => { ctx.cache[i] = obj; return i; },
            _ => ()
        }
    }
    ctx.cache.push(obj);
    ctx.cache.len() - 1
}
fn ok_obj<'a>(ctx: &'a mut Context, obj: Object) -> Option<Term> {
    ok_i32(cache_insert(ctx, obj) as i32)
}
// Lifetimes of context and term are different.  Rust requires them to
// be declared such that it knows which of the two is the lifetime of
// the output.
fn as_slot<'a,'b>(ctx: &'a mut Context, term: &'b Term) -> Option<&'a mut Object> {
    let index = as_i32(term)?;
    ctx.cache.get_mut(index as usize)
}
fn as_etflog<'a,'b> (ctx: &'a mut Context, term: &'b Term) -> Option<&'a etflog::EtfLog> {
   match as_slot(ctx, term)? {
       Object::EtfLog { obj: rl } => Some(rl), _ => None
   }
} 
fn as_samplestore<'a,'b> (ctx: &'a mut Context, term: &'b Term) -> Option<&'a samplestore::SampleStore> {
   match as_slot(ctx, term)? {
       Object::SampleStore { obj: rl } => Some(rl), _ => None
   }
}

fn dispatch(ctx: &mut Context,
            cmd: &Term) -> Option<Term> {
    ctx.count += 1;
    match as_str(cmd) {
        Some(cmd) =>
            match cmd {
                "exit" => {
                    process::exit(0)
                }
                "count" => {
                    ok_i32(ctx.count as i32)
                },
                "cache_size" => {
                    ok_i32(ctx.cache.len() as i32)
                },
                _ => None
            },
        None => {
            let v = as_vec(cmd, Some(2))?;
            let cmd = as_str(&v[0])?;
            let arg = &v[1];
            match cmd { // {atom(), _}
                "open" => {
                    let arg = as_vec(arg, Some(2))?;
                    let typ = as_str(&arg[0])?;
                    let arg = &arg[1] ;
                    match typ {
                       "etflog" => {
                           let filename  = as_str(&arg)?.to_string();
                           let log = etflog::EtfLog::open(&filename)?;
                           ok_obj(ctx, Object::EtfLog { obj: Box::new(log) })
                       },
                       "samplestore" => {
                           let filename  = as_str(&arg)?.to_string();
                           let store = samplestore::SampleStore::open(&filename)?;
                           ok_obj(ctx, Object::SampleStore { obj: Box::new(store) })
                       },
                       "jack_client" => {
                           let client = jack_client::start(as_str(&arg)?);
                           ok_obj(ctx, Object::JackClient { obj: Box::new(client) })
                       },
                        _ => None
                    }
                }
                "close" => {
                    let index = as_i32(&arg)? as usize;
                    if index >= ctx.cache.len() {
                        None
                    }
                    else {
                        ctx.cache[index] = Object::Empty;
                        ok_i32(index as i32)
                    }
                }
                _ => None
            }
        }
    }
}


fn ok()                  -> Option<Term> { Some(atom("ok")) }
fn ok_term(term: Term)   -> Option<Term> { Some(tag("ok", term)) }
fn ok_i32(val: i32)      -> Option<Term> { ok_term(i32(val)) }
fn ok_binary(buf: &[u8]) -> Option<Term> { ok_term(binary(buf)) }
fn ok_atom(buf: &str)    -> Option<Term> { ok_term(atom(buf)) }

fn ok_i32_vec(v: &Vec<i32>)        -> Option<Term> { ok_term(i32_vec(v)) }
fn ok_i32_vec2(vv: &Vec<Vec<i32>>) -> Option<Term> { ok_term(i32_vec2(vv)) }


// fn ok_atom(buf: &str) -> Option<Term> { Some(tag("ok", atom(buf))) }
// fn error_io(err: io::Error) -> Option<Term> {
//     let msg = format!("{:?}",err);
//     Some(tag("error", tag("io", binary(msg.as_bytes()))))
// }


// (exo@10.1.3.2)10> timer:tc(fun() -> exo_rs:call({etflog_save_index_u32,{<<"/vol/cams/data.42">>, <<"/vol/cams/index.42">>}}) end).

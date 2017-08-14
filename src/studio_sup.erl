-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1,
         set_cookie/0
        ]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

set_cookie() ->
    {ok, Bin} = file:read_file(".cookie"),
    erlang:set_cookie(node(), binary_to_atom(Bin,utf8)).

init([]) ->
    %% use .erlang.cookie instead
    set_cookie(),
    {ok, {{one_for_one, 1, 5},
          [worker(permanent, midi_jack, midi, jackd_port_start_link,[]),
           worker(permanent, midi_hub,  midi, hub_start_link,[]),
           worker(permanent, db,        db,   start_link,[])]}}.


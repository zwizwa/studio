-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

init([]) ->
	{ok, {{one_for_one, 1, 5},
              [worker(permanent, midi_jack, midi, jackd_port_start_link,[]),
               worker(permanent, midi_hub,  midi, hub_start_link,[]),
               worker(permanent, db,        db,   start_link,[])]}}.

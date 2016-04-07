-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Mod,Fun,Args) ->
    {Mod,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

init([]) ->
	{ok, {{one_for_one, 1, 5},
              [worker(permanent,midi,jackd_port_start_link,[])]}}.

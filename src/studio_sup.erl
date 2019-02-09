-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

init([]) ->
    %% use .erlang.cookie instead
    %% set_cookie(),
    case os:cmd("echo -n $(which jackd.$(hostname))") of
        [] ->
            tools:info("WARNING: no local jackd config\n"),
            {ok, {{one_for_one, 1, 5},
                  [worker(permanent, midi_hub,  midi, hub_start_link,[])]}};
        _ ->
            {ok, {{one_for_one, 1, 5},
                  [worker(permanent, midi_jack, midi, jackd_port_start_link,[])
                  ,worker(permanent, midi_hub,  midi, hub_start_link,[])]}}
    end.

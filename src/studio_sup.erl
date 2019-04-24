-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, jackd/0, find/1, studio_elf/0,
         midi_hub/0, midi_jack/0, restart_port/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

jackd() ->
    os:cmd("echo -n $(which jackd.$(hostname))").

init([]) ->
    %% use .erlang.cookie instead
    %% set_cookie(),
    case jackd() of
        [] ->
            tools:info("WARNING: no local jackd config\n"),
            {ok, {{one_for_one, 1, 5},
                  [worker(permanent, midi_hub,  midi, start_link,[])]}};
        _ ->
            {ok, {{one_for_one, 1, 5},
                  [worker(permanent, midi_hub,  midi, start_link,[]),
                   %% Note that the main process here is a wrapper
                   %% around jackd, which spawns jack client process
                   %% wrappers as children once jackd is up.
                   worker(permanent, midi_jack, jack_daemon, start_link,[])
                  ]}}
    end.

find(Name) ->
    find(Name, supervisor:which_children(studio_sup)).
find(_,[]) -> error;
find(Name,[{N,Pid,_,_}|Rest]) -> 
    case N of
        Name -> {ok, Pid};
        _ -> find(Name, Rest)
    end.

midi_hub()  -> {ok, Pid} = find(midi_hub), Pid.
midi_jack() -> {ok, Pid} = find(midi_jack), Pid.
    
            
studio_elf() ->
    %% Old style
    %% code:priv_dir(studio) ++ "/studio.elf".
    %% New style: exo build deploys here:
    os:getenv("HOME") ++ "/bin/studio.elf".

%% Custom restart code for port binaries, called from exo
%% push_change.erl
restart_port(BaseName) ->
    log:info("studio_sup:restart_port: ~p~n", [BaseName]),
    case BaseName of
        <<"studio">> ->
            obj:gets(midi_jack(),[midi]) ! restart_port
    end.
    

%% likely jack and pulseaudio don't mix.  depends on host.
%% turn jack on and off and on like this:
%% supervisor:terminate_child(studio_sup, midi_jack).
%% supervisor:restart_child(studio_sup, midi_jack).


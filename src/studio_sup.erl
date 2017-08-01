-module(studio_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_ssh/0, start_ssh_shell/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

worker(Restart,Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},Restart,brutal_kill,worker,[Mod]}.

init([]) ->
	{ok, {{one_for_one, 1, 5},
              [worker(permanent, midi_jack, midi, jackd_port_start_link,[]),
               worker(permanent, midi_hub,  midi, hub_start_link,[]),
               worker(permanent, db,        db,   start_link,[])]}}.


start_ssh_shell(_User, _PeerAddr) ->
    %%GL=erlang:group_leader(),
    %%tools:info_subscribe(GL),
    %%tools:info("ssh: ~p~n", [{_User,_PeerAddr}]),  %% Things causes shell to hang.
    shell:start([]).
    

start_ssh() ->
    application:ensure_all_started(ssh),
    %% Shell is only started if no ssh command is provided.
    ssh:daemon(
      2222, 
      [{shell,       fun studio_sup:start_ssh_shell/2},
       {system_dir,  priv_dir("ssh_daemon")},
       {user_dir,    priv_dir("ssh_user/.ssh")}]).

priv_dir(D) ->
    tools:format("priv/~s", [D]).

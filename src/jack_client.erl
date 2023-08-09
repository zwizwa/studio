%% Standardized wrapper for synth_tools jack clients.
%% These all have a midi port on stdio.

-module(jack_client).
-export([%% run_udp/1, handle_udp/2,
         proc/1, handle_proc/2
        ]).

%% Start a processor or synth.
proc(#{ name := Name,  %% basename
        spawn_port := _SpawnPort }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name({rai,Name}),
                Self = self(),
                Self ! start,
                %% By default, take the processors that are compiled
                %% as part of synth_tools package Nixos package.
                Dir = tools:format("~s/linux",
                                   %% ["/i/exo/synth_tools"]
                                   [os:getenv("SYNTH_TOOLS")]),
                maps:merge(
                  #{ dir => Dir},
                  Config)
        end,
        fun ?MODULE:handle_proc/2})}.

spawn_params(Dir, Cmd) ->
    #{ dir  => Dir,
       cmd  => Cmd,
       args => [],
       opts => [use_stdio, binary, exit_status,
                {packet,4}] %% FIXME
     }.

handle_proc(start, #{ name := Name, spawn_port := SpawnPort, dir := Dir } = State) ->
    case maps:find(port, State) of
        {ok, _Port} ->
            State;
        error ->
            Cmd = tools:format("~s.dynamic.host.elf",[Name]),
            log:info("Cmd = ~p~n", [Cmd]),
            maps:put(
              port,
              SpawnPort(spawn_params(Dir,Cmd)),
              State)
    end;

handle_proc(stop, State) ->
    log:info("stop\n"),
    case maps:find(port, State) of
        {ok, Port} ->
            port_close(Port);
        error ->
            ok
    end,
    maps:remove(port, State);

%% FIXME: Add restart with path
handle_proc(restart, State) ->
    lists:foldl(
      fun(Cmd, S) -> handle_proc(Cmd, S) end,
      State,
      [stop, start]);

handle_proc({Port,{exit_status,_}=E}, State = #{port := Port}) ->
    %% Don't crash the process, just issue a warning.
    log:info("WARNING: ~p~n",[E]),
    maps:remove(port, State);

handle_proc(Msg={_,dump},State) ->
    obj:handle(Msg, State);

%% Delegate to mixins at tail end.
handle_proc(Msg, State) -> 
    Mixins = [fun epid:mixin/3, fun tag_u32:mixin/3],
    {Handled, State1} = serv:delegate(Mixins, Msg, State),
    case Handled of
        true ->
            State1;
        false ->
            log:info("unknown: ~p~n",[Msg]),
            State1
    end.



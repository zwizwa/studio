-module(jack_client).
-export([%% run_udp/1, handle_udp/2,
         proc/1, handle_proc/2
        ]).

%% Stripped down version of studio/src/rai.erl
%% Dumb wrapper around stand-alone jack binary.

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
                Config
        end,
        fun ?MODULE:handle_proc/2})}.

handle_proc(start, #{ name := Name, spawn_port := SpawnPort } = State) ->
    case maps:find(port, State) of
        {ok, _Port} ->
            State;
        error ->
            Cmd = tools:format("~s.dynamic.host.elf",[Name]),
            log:info("Cmd = ~p~n", [Cmd]),
            maps:put(
              port,
              SpawnPort(
                #{ dir  => "/i/exo/synth_tools/linux",
                   cmd  => Cmd,
                   args => [],
                   opts => [use_stdio, binary, exit_status,
                            {packet,4}] %% FIXME
                 }),
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



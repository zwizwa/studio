%% FIXME: The name "jack_client" does not cover it.
%%
%% This is really "Erlang Midi Client" or something, i.e. anything
%% that talks the protocol on stdin.

%% Standardized wrapper for synth_tools jack clients.
%% These all have a midi port on stdio.

-module(jack_client).
-export([%% run_udp/1, handle_udp/2,
         proc/1, handle_proc/2
        ]).

-define(TAG_STREAM, 16#FFFB).
-define(TAG_PTERM,  16#FFEE).
-define(TAG_U32,    16#FFF5).

%% Start a processor or synth.
proc(#{ name := Name,  %% basename
        spawn_port := _SpawnPort }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name({jack_client,Name}),
                Self = self(),
                Self ! start,
                %% By default, take the processors that are compiled
                %% as part of synth_tools package Nixos package.
                Dir = tools:format("~s/linux",
                                   %% ["/i/exo/synth_tools"]
                                   [os:getenv("SYNTH_TOOLS")]),
                maps:merge(
                  #{ dir => Dir,
                     tape => []
                   },
                  Config)
        end,
        fun ?MODULE:handle_proc/2})}.

spawn_params(Dir, Cmd) ->
    #{ dir  => Dir,
       cmd  => Cmd,
       args => [],
       opts => [use_stdio, binary, exit_status, {packet,4}] %% FIXME
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
            port_close(Port),
            %% Sleep workaround to make sure jack sees the unregister
            %% before the register.  The right way to do this is to
            %% wait for the unreg event, but that is a lot of work to
            %% implement.
            timer:sleep(250),
            ok;
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

handle_proc({set_dir, Dir}, State) ->
    maps:put(dir, Dir, State);

handle_proc({midi, PortNb, Midi}, State = #{ port := Port }) when is_binary(Midi) ->
    %% Use TAG_STREAM for midi ports.
    %% FIXME: Use a dedicated format for the old 8-bit port + stamp format.
    Msg = <<?TAG_STREAM:16, PortNb:16, Midi/binary>>,
    exo:info("jack_client: midi_from_erl ~p ~s~n", [PortNb, tools:hex(Midi)]),
    port_command(Port, Msg),
    State;
handle_proc({midi, PortNb, Cmd}, State) ->
    F = fun(Midi) ->
                handle_proc({midi, PortNb, Midi}, State)
        end,
    case Cmd of
        start    -> F(<<16#FA>>);
        continue -> F(<<16#FB>>);
        stop     -> F(<<16#FC>>);
        _ -> State
    end;


handle_proc({midi, Midi}, State) ->
    handle_proc({midi, 0, Midi}, State);

%% FIXME: Add synchronous restart RPC.
%% This will restart and send a reply when device sends 0xFE after startup.
handle_proc({ReplyTo, restart}, State) ->
    maps:put(ping_reply_to, ReplyTo, State);

%% exo:pid({jack_client,<<"jack_akai_fire">>}) ! {set_dir, "/i/exo/synth_tools/linux"}.

handle_proc({Port,{data,<<255,253>>}},
            State = #{port := Port, ping_reply_to := ReplyTo}) ->
    obj:reply(ReplyTo, ok),
    maps:remove(ping_reply_to, State);

handle_proc({Port,{data,<<?TAG_PTERM:16,Pterm/binary>>}},
            State = #{port := Port}) ->
    Term = type:decode({pterm,Pterm}),
    log:info("jack_client: pterm: ~p~n", [Term]),
    TapeStack = maps:get(tape, State, []),
    case Term of
        {record, start} ->
            maps:put(tape, [], State);
        {record, stop} ->
            Tape = lists:reverse(TapeStack),
            log:info("Tape=~p~n", [Tape]),
            Seq = studio_seq:split_loop(Tape),
            NbClocks = 24 * 2,
            SeqClock = studio_seq:time_scale(NbClocks, Seq),
            Pattern = studio_seq:pattern(0, SeqClock),
            log:info("Pattern=~p~n", [Pattern]),
            %% Spawn temp task to RPC into this object
            Pid = self(),
            spawn(
              fun() ->
                      lists:foreach(
                        fun(Cmd) -> tag_u32:call(Pid, Cmd) end,
                        Pattern)
              end),
            maps:put(tape, [], State);
        {record, Cmd} ->
            maps:put(tape, [Cmd|TapeStack], State);
        _ ->
            State
    end;

handle_proc({Port,{data,<<?TAG_STREAM:16,MidiPort:16,Midi/binary>>}},
            State = #{port := Port}) ->
    log:info("jack_client: midi_to_erl: ~p ~s~n", [MidiPort, tools:hex(Midi)]),
    case MidiPort of
        _ ->
            ok
    end,
    State;

%% Mixins will also handle Port data so don't add a catch-all here.
%handle_proc({Port,{data,Data}},
%            State = #{port := Port}) ->
%    log:info("jack_client: unknown: ~p~n", [Data]),
%    State;

handle_proc({Port,{exit_status,_}=E}, State = #{port := Port}) ->
    %% Don't crash the process, just issue a warning.
    %% log:info("WARNING: ~p~n",[E]),
    maps:remove(port, State);

handle_proc(Msg={_,dump},State) ->
    obj:handle(Msg, State);


%% Delegate to mixins at tail end.
handle_proc(Msg, State) -> 
    Mixins = [fun epid:mixin/3,
              fun tag_u32:mixin/3],
    {Handled, State1} = serv:delegate(Mixins, Msg, State),
    case Handled of
        true ->
            State1;
        false ->
            log:info("jack_client: unknown: ~p~n",[Msg]),
            State1
    end.



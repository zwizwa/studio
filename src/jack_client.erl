%% To simplify:
%%
%% 1. All jack clients speak the uc_tools packet tag protocol, and
%%    exit when stdin is closed (standard Erlang port behavior).
%%
%% 2. All jack clients are implemented as synth_tools/linux/*.c
%%    compiled to .dynamic.host.elf
%%
%% 3. The "distribution" version has these installed in /nix/store,
%%    while the development version uses /i/exo/
%%
%% 4. External jack clients (e.g. a2jmidid or Pd) use a wrapper .c
%%    file that translates between the jack_client.erl protocol and
%%    whatever the application needs.


-module(jack_client).
-export([%% run_udp/1, handle_udp/2,
         proc/1, handle_proc/2,
         %% RPC
         start/1,
         stop/1,
         restart/1,
         program/2
        ]).

-define(TAG_INFO,   16#FFFE).
-define(TAG_STREAM, 16#FFFB).
-define(TAG_PTERM,  16#FFEE).
-define(TAG_U32,    16#FFF5).

%% Jack control
-define(CMD_CONNECT,1).
-define(CMD_DISCONNECT,2).

%% Start a processor or synth.
proc(#{ name := Name,  %% basename
        spawn_port := _SpawnPort }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name({jack_client,Name}),
                Self = self(),
                %% This has changed.  All synth_tools binaries are no
                %% longer part of the "system", but are referred to
                %% here using late binding of host-local
                %% ~/.result/synth_tools which is used as the
                %% gc-registered output of /etc/net/bin/nix.build.sh
                Dir = tools:format("~s/linux",
                                   [[os:getenv("HOME"),"/.result/synth_tools"]]
                                   %% ["/i/exo/synth_tools"]
                                   %% [os:getenv("SYNTH_TOOLS")]
                                  ),
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
       opts => [use_stdio, binary, exit_status, {packet,4}]
     }.

handle_proc({Pid, start},
            #{ name := Name,
               spawn_port := SpawnPort,
               dir := Dir } = State) ->
    State1 = 
        case maps:find(port, State) of
            {ok, _Port} ->
                %% Idempotent: already started.
                State;
            error ->
                Cmd = tools:format("~s.dynamic.host.elf",[Name]),
                log:info("Cmd = ~p~n", [Cmd]),
                Port = SpawnPort(spawn_params(Dir,Cmd)),
                %% FIXME: This delay should be replaced by an RPC to
                %% the client, to ensure all external communication
                %% functionality (Jack connection, TCP service, ...)
                %% is up.
                timer:sleep(250),
                maps:put(port, Port, State)
        end,
    obj:reply(Pid, {ok, maps:get(port, State1)}),
    State1;

handle_proc({Pid, stop}, State) ->
    log:info("stop\n"),
    State1 = 
        case maps:find(port, State) of
            {ok, Port} ->
                port_close(Port),
                %% FIXME: This delay should be replaced by an RPC to the
                %% client, to ensure all external communication
                %% functionality (Jack connection, TCP service, ...) is
                %% down.
                timer:sleep(250),
                maps:remove(port, State);
            error ->
                %% Idempotent: already stopped.
                State
        end,
    obj:reply(Pid, ok),
    State1;

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
        play     -> F(<<16#FA>>);
        continue -> F(<<16#FB>>);
        stop     -> F(<<16#FC>>);
        _ -> State
    end;


handle_proc({connect, Src, Dst}=_Msg, State) ->
    Bin = tools:format_binary("~s~s", [Src, Dst]),
    Pid = self(),
    Cmd = [jack_port, ?CMD_CONNECT, size(Src), size(Dst)],
    spawn(fun() -> tag_u32:call(Pid, Cmd, Bin) end),
    State;

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

handle_proc({Port,{data,<<?TAG_INFO:16, Log/binary>>}},
            State = #{port := Port}) ->
    log:info("info: ~s", [Log]),
    State;

handle_proc({Port,{data,<<?TAG_PTERM:16,Pterm/binary>>}},
            State = #{port := Port}) ->
    Term = type:decode({pterm,Pterm}),
    %% log:info("jack_client: pterm: ~p~n", [Term]),
    TapeStack = maps:get(tape, State, []),
    case Term of
        {record, start} ->
            maps:put(tape, [], State);
        {record, stop} ->
            Tape = lists:reverse(TapeStack),
            log:info("Tape=~p~n", [Tape]),
            Seq = studio_seq:split_loop(Tape),
            NbClocks = 24 * 2,
            {Div,Pattern} = studio_seq:time_scale(NbClocks, Seq),
            PatternBin = studio_seq:pattern_pack(Pattern),
            %% Spawn temp task to RPC into this object
            Pid = self(),
            spawn(fun() -> studio_seq:load_pattern(Pid, PatternBin) end),
            maps:put(tape, [], State);
        {record, Cmd} ->
            maps:put(tape, [Cmd|TapeStack], State);
        {jack_control, ControlMsg} ->
            %% Forward it to jack_daemon which is tied into the rest
            %% of the system.  It seems safe to use the registered
            %% name here.
            %% log:info("jack_client: jack_control: ~999p~n",[ControlMsg]),
            jack_daemon ! Term,
            State;
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



start(Pid) -> obj:call(Pid, start).
stop(Pid)  -> obj:call(Pid, stop).
restart(Pid) -> stop(Pid), start(Pid).

program(Pid, Program) ->   
    lists:foreach(
      fun({Cmd,Bin}) ->
              %% Not sure what to do with errors, so let it crash on a
              %% match error here.
              case tag_u32:call(Pid, Cmd, Bin) of
                  {[0|_],_} ->
                      %% Each call needs to complete.
                      ok
              end;
         (Cmd) ->
              %% Not sure what to do with errors, so let it crash on a
              %% match error here.
              case tag_u32:call(Pid, Cmd) of
                  {[0|_],_} ->
                      %% Each call needs to complete.
                      ok
              end
      end,
      Program).
    

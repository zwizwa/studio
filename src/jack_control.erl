-module(jack_control).
-export([start_link/1, handle/2,
        ports/0, ports/1, clients/0]).

start_link(#{client := Client, notify := _Notify}=Config) ->
    Pid = 
        serv:start(
          {handler, 
           fun() ->
                   log:set_info_name(jack_control),
                   register(jack_control, self()),
                   %% log:set_info_name({jack_control, Client}),
                   Cmd = tools:format("~s jack_control ~s", [jack_daemon:studio_elf(), Client]),
                   Args = [{spawn,Cmd},[{packet,1},binary,exit_status]],
                   handle(
                     restart_port,
                     maps:put(open_port, Args, Config))
           end,
           fun ?MODULE:handle/2}),
    {ok, Pid}.

%% Jack control client
-define(CMD_CONNECT,1).
-define(CMD_DISCONNECT,2).

fmt_port(Bin) when is_binary(Bin) ->
    Bin;
fmt_port({C,P}) ->
    tools:format_binary("~s:~s",[C,P]).

     

handle(restart_port, State = #{ open_port := Args }) ->
    case maps:find(port, State) of
        {ok, Port} ->
            Port ! {self(), {command, <<>>}},
            receive {Port, {exit_status, _}}=_E -> ok
            after 3000 -> exit({restart_port_timeout, Args})
            end;
        _ ->
            ok
    end,
    log:info("start: ~p~n", [Args]),
    maps:put(port, apply(erlang, open_port, Args), State);

%% Protocol is asynchronous.  This makes it easier to use the return
%% pipe for jack events.
handle({connect, Src, Dst}, State) ->
    handle({rewire,?CMD_CONNECT,Src,Dst}, State);
handle({disconnect, Src, Dst}, State) ->
    handle({rewire,?CMD_DISCONNECT,Src,Dst}, State);

%% Also support the epid protocol.
handle({epid_send,Port,Msg}=EpidSend, State) ->
    %% Use canonical names.
    log:info("epid command: ~p~n", [EpidSend]),
    Self = self(),
    case Msg of
        %% Internal connections.  Map to canonical names and delegate.
        {epid_subscribe, {epid,Self,DstPort}} ->
            handle({connect,
                    portname(Port),
                    portname(DstPort)},
                   State);
        {epid_unsubscribe, {epid,Self,DstPort}} ->
            handle({disconnect,
                    portname(Port),
                    portname(DstPort)},
                   State);
        _ ->
            %% Anything else is handled by the high level midi hub.
            %% FIXME: Remove hardcoded name
            jack_midi ! EpidSend,
            %% log:info("Bad epid command: ~p~n", [_EpidSend]),
            State
    end;

handle({rewire, RewireKind, Src0, Dst0} = _Msg, State = #{ port := Port }) ->
    log:info("~999p~n", [_Msg]),
    Src = fmt_port(Src0),
    Dst = fmt_port(Dst0),
    Cmd = <<RewireKind,Src/binary,0,Dst/binary,0>>,
    Port ! {self(), {command, Cmd}},
    State;

handle({Port,{exit_status,_}=E}, _State = #{ port := Port }) ->
    exit(E);


handle({Pid, ports}, State) ->
    %% This is a little messy, but currently works
    obj:reply(Pid, [P || {_,_}=P <- maps:keys(State)]),
    State;


handle({Port,{data, Data}}, State = #{ port := Port, notify := Notify }) ->
    %% Protocol is pterm wrapped in {packet,1}, which is easy to
    %% generate in C and easy to parse here.  FIXME: probably best to
    %% switch to {packet,2} or {packet,4}
    Parsed = type:decode({pterm, Data}),
    log:info("~999p~n", [Parsed]),

    %% FIXME: hardcoded
    Notify(Parsed),

    case Parsed of
        {port,Active,CP} ->
            {C,P} = studio_db:port_pair(CP),
            maps:put({C,P},Active,State);
        %% {alias,"system:midi_playback_4","out-hw-4-0-2-USB-Midi-4i4o-MIDI-3"} -> ok;
        %% {port,true,"system:midi_playback_5"} -> ok;
        %% {connect, true,  _A, _B} -> State;
        _ ->
            State
    end;

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).


%% Internally we use names not numbers, but the interface supports
%% both formats.
portname({port, PortDir, PortNb}) ->
    {ok, HwPort} = jack_daemon:system_port(jack_daemon, PortDir, PortNb),
    HwPort;
portname(Name) when is_binary(Name) ->
    Name.


%% Absence of daemon can be mapped to empty collections.
call_default(Msg,Dflt) ->
    case whereis(?MODULE) of
        undefined -> Dflt;
        Pid -> obj:call(Pid, Msg)
    end.

ports() ->
    call_default(ports, []).
ports(C) ->
    lists:filter(fun({C0,_}) -> C0 == C end, ports()).
clients() ->
    tools:unique([C || {C,_} <- ports()]).


             

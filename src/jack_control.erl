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
    tools:format_binary("~s:~s",[C,P]);
fmt_port(IOL) ->
    tools:format_binary("~s",[IOL]).


     

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
%% This supports:
%% - from arbitrary external epid to jack port
%% - between jack ports
%%
%% A jack port can be a system port or a client port.  We need both
%% because client ports need to look like system ports.


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

    %% log:info("~999p~n", [Parsed]),

    %% Here I want the following: allow "wait for notification", where
    %% a process can temporarily register the bringup of a port, and
    %% receive a single message when the port appears.

    %% Alternatively, let the jack client send a proper notification.
    %% It's much easier to implement from that end.

    %% Also, we could easily keep track of which processors are up and
    %% what the io ports are.

    Notify(Parsed),

    case Parsed of
        %% Is this still necessary?
        %% RegUnreg = reg | unreg
        %% InOut = in | out
        {port,RegUnreg,InOut,CP} ->
            {C,P} = studio_cfg:port_pair(CP),
            maps:put({InOut,C,P},RegUnreg,State);
        %% {alias, "system:midi_playback_4", "out-hw-4-0-2-USB-Midi-4i4o-MIDI-3"} -> ok;
        %% {port, reg, out, "system:midi_playback_5"} -> ok;
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


    

    
%% Wait for client coming up by:
%% exo_midi:jack_notify({client,true,"jack_synth-01"})
%% exo_midi:jack_notify({port,true,"jack_synth-01:midi_in_0"})
%% exo_midi:jack_notify({port,true,"jack_synth-01:audio_out_0"})

wait(Client,Ports) ->
    %% FIXME: Register
    Ref = erlang:make_ref(),
    receive {Tag, {client,true,Name}} -> ok end,
    [begin 
         PortName = tools:format("~s:~s",[Name,Port]),
         receive {Tag, {port,true,PortName}} -> ok end
     end || Port <- Ports],
    %% FIXME: Flush
    ok.

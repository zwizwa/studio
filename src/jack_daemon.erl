%% Jack Daemon wrapper.  Note that this insists on managing the
%% daemon.  I.e. it is "framework-y".

%% Some breadcrumbs:
%%
%% - In exo, this runs in the main supervisor.  Only jack_daemon is
%%   stared, which then starts other functionality.
%%
%% - Exo also starts midi_raw, which is independent of all jack code.
%%   Its purpose is to be a hub for midi devices that are not
%%   connected to jack.
%%
%% - While jack daemon is starting up, the daemon's stdout is parsed
%%   and for each 'added' line, handle_connect/4 is called, which...
%%
%% - ... lazy starts jack_control, jack_midi and jack_audio clients.
%%
%% - jack_control handles port/alias events
%%
%% - jack_midi is a C port and Erlang wrapper that does some "data
%%   plane" midi operations (e.g. clock generation, sequencer, sysex)
%%   inside the C application, and furthermore bridges the Jack MIDI
%%   world and the Erlang message world.  Some thought has been put in
%%   here, so have a look at jack_midi.c
%%
%% - jack_audio is currently a dummy memcpy audio sink



%% Notes
%%
%% - This evolved in a very ad-hoc way.  I currently do not have the
%%   time to redesign it.  I guess it is ok, just that startup is a
%%   little messy.
%%
%% - It is probably possible to remove the stdout parsing, but at this
%%   time it is still used to ensure the jack clients are only started
%%   once the daemon is up.  Once control deamon is up, events are
%%   handled that way.


-module(jack_daemon).
-export([start_link/1, handle/2, studio_elf/0, start_client/2, system_port/3]).


%% Wrap the daemon and listen on its stdout as a simple way to get
%% MIDI port connect notifications.

%% Once midi port aliases are known, connect them to a specified port
%% number on the jack client.

start_link(Init = #{ hubs := _}) ->
    {ok, serv:start(
           {handler,
            fun() -> 
                    log:set_info_name(?MODULE),
                    %% timer:send_after(2000, start),
                    self() ! start,
                    Init
            end,
            fun ?MODULE:handle/2})}.

handle(start, State) ->
    SH = code:priv_dir(studio) ++ "/start_jackd.sh",
    tools:info("jackd_open: ~s~n",[SH]),
    Opts = [{line,1024}, binary, use_stdio, exit_status],
    Port = open_port({spawn, SH}, Opts),
    maps:put(port, Port, State);

handle({Port, {data, Data}}, #{port := Port} = State) ->
    case Data of
        {eol, Line} ->
            log:info("~s~n", [Line]),
            handle({line, Line}, State);
        _ ->
            log:info("UNEXPECTED: Data=~p~n", [Data])
    end;
handle({Port, {exit_status, _}}=Msg, State = #{ port := Port }) ->
    log:info("WARNING: ~p~n", [Msg]),
    timer:send_after(2000, start),
    maps:remove(port, State);



%% This requires some explanation.  jackd will emit "scan:" lines such
%% as is printed out above the regexp below.  Note that the numbering
%% scheme is not likely to be stable across restarts, so we use only
%% the human readable name part.
%%
%% %% The 'scan:' lines:
%%
%% scan: added port hw:1,0,0 in-hw-1-0-0-UMA25S-MIDI-1
%% scan: added port hw:1,0,0 out-hw-1-0-0-UMA25S-MIDI-1
%% scan: added port hw:3,0,0 in-hw-3-0-0-FastTrack-Pro-MIDI-1
%% scan: added port hw:3,0,0 out-hw-3-0-0-FastTrack-Pro-MIDI-1
%% scan: opened port hw:1,0,0 in-hw-1-0-0-UMA25S-MIDI-1
%% scan: opened port hw:1,0,0 out-hw-1-0-0-UMA25S-MIDI-1
%% scan: opened port hw:3,0,0 in-hw-3-0-0-FastTrack-Pro-MIDI-1
%% scan: opened port hw:3,0,0 out-hw-3-0-0-FastTrack-Pro-MIDI-1
%%
%% %% Are parsed to:
%%
%% added  {<<"in">>, <<"UMA25S-MIDI-1">>}        => <<"in-hw-1-0-0-UMA25S-MIDI-1">>
%% added  {<<"out">>,<<"UMA25S-MIDI-1">>}        => <<"out-hw-1-0-0-UMA25S-MIDI-1">>
%% added  {<<"in">>, <<"FastTrack-Pro-MIDI-1">>} => <<"in-hw-3-0-0-FastTrack-Pro-MIDI-1">>
%% added  {<<"out">>,<<"FastTrack-Pro-MIDI-1">>} => <<"out-hw-3-0-0-FastTrack-Pro-MIDI-1">>
%% opened {<<"in">>, <<"UMA25S-MIDI-1">>}        => <<"in-hw-1-0-0-UMA25S-MIDI-1">>
%% opened {<<"out">>,<<"UMA25S-MIDI-1">>}        => <<"out-hw-1-0-0-UMA25S-MIDI-1">>
%% opened {<<"in">>, <<"FastTrack-Pro-MIDI-1">>} => <<"in-hw-3-0-0-FastTrack-Pro-MIDI-1">>
%% opened {<<"out">>,<<"FastTrack-Pro-MIDI-1">>} => <<"out-hw-3-0-0-FastTrack-Pro-MIDI-1">>
%%
%% The PortAlias is what is what Jack uses to identify the port.  Note
%% that the hw numbering is not stable, so we keep track of a map from
%% a stable name to this name used by jack to make connections.
%%

handle({line, <<"scan: ", Rest/binary>>=_Line}, State) ->
    %% tools:info("~s~n",[_Line]),
    {match,[_|[Action,_HwAddr,Dir,Addr,Name]]} =
        re:run(
          Rest,
          <<"(\\S+) port (\\S+) (\\S+)\\-(hw\\-\\d+\\-\\d+\\-\\d+)\\-(\\S+)\n*">>,
          [{capture,all,binary}]),
    PortAlias = <<Dir/binary,$-,Addr/binary,$-,Name/binary>>,
    Key = {Dir, Name},
    %% log:info("~s ~p => ~p~n",[Action, Key, PortAlias]),
    case Action of
        <<"added">> ->
            %% S1=maps:put(Key,PortAlias,State),
            S1=State,
            handle_connect(PortAlias, Dir, Name, S1);
        <<"deleted">> ->
            maps:remove(Key, State);
        _ ->
            State
    end;
handle({line, _Line}, State) -> 
    %% tools:info("~s~n",[_Line]),
    State;
handle({client, Msg}, State) ->
    case Msg of
        {_,tc} -> dont_print;
        _ -> tools:info("jack client message: ~p~n",[Msg])
    end,
    State;

handle(Msg={_, {find, _}}, State) ->
    obj:handle(Msg, State);

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).


handle_connect(PortAlias, Dir, Name, State) ->

    %% Client can be started only after daemon is up, so do it lazily.
    {Control, State1} = control_client(State),

    N = studio_cfg:port_id(Name),
    NBin = integer_to_binary(N),
    %% tools:info("~p~n",[[PortAlias,Dir,Name,N]]),
    Connect =
        fun(Src,Dst) ->
                spawn(
                  fun() ->
                          %% Port creation seems to happen after it is
                          %% logged to the console.  Can't sync, so
                          %% add a workaround timeout.
                          timer:sleep(500),
                          Control ! {connect,Src,Dst}
                  end)
        end,

    %% Tell controller to connect, but keep the association locally as
    %% well to be able to resolve {Dir,N} -> PortAlias later for other
    %% purposes.
    case Dir of
        <<"in">>  ->
            Connect(PortAlias,<<"studio_midi:midi_in_",NBin/binary>>),
            maps:put({in,N},PortAlias,State1);
        <<"out">> ->
            Connect(<<"studio_midi:midi_out_",NBin/binary>>,PortAlias),
            maps:put({out,N},PortAlias,State1)
    end.


control_client(State) ->
    State1 = need_clients(State),
    {maps:get(control, State1), State1}.

need_clients(State = #{ control := _, midi := _ }) -> 
    State;
need_clients(State) ->
    tools:info("starting clients~n"),
    maps:merge(
      State,
      maps:from_list(
        [{Name,start_client(Name, State)}
         || Name <- [control, midi, audio]])).

start_client(Name, #{ hubs := Hubs, notify := Notify}) ->
    {ok, Pid} = 
        case Name of
            control -> jack_control:start_link(
                         #{client => "studio_control",
                           notify => Notify
                          });
            midi    -> jack_midi:start_link(
                         #{ hubs => Hubs,
                            client => "studio_midi",
                            midi_ni => 24, 
                            midi_no => 24,
                            clock_mask => studio_cfg:midiclock_mask() });
            audio   -> jack_audio:start_link("studio_audio", 8)
        end,
    Pid.


system_port(Pid,Dir,N) when is_number(N) ->
    obj:find(Pid, {Dir,N}).
                    

studio_elf() ->
    Elf = code:priv_dir(studio) ++ "/studio.elf",
    log:info("Elf=~p~n", [Elf]),
    Elf.

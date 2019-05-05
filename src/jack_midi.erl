%% Jack midi interface.

%% While this is much better than alsa_midi, please note that Erlang
%% is too jittery to have really good timing.  It is best to treat
%% midi processing code as "datapath" code, and use the Erlang bridge
%% only for convenience.

-module(jack_midi).
-export([start_link/4, handle/2]).

-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).

%% JACK midi client. Preferred as it has better timing properties.
%% Supports midi in/out, clock generation, and jack client connection.
-define(JACK_MIDI_CMD_MIDI,0).
-define(JACK_MIDI_CMD_CONNECT,1).

start_link(Client,MidiNI,MidiNO,ClockMask) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                register(jack_midi, self()),
                Cmd = 
                    tools:format(
                      "~s jack_midi ~s ~p ~p ~p",
                      [studio_sup:studio_elf(),
                       Client, MidiNI, MidiNO, ClockMask]),
                OpenPort =
                    [{spawn,Cmd},
                     [{packet,4}
                     ,binary,exit_status]],
                log:set_info_name({jack_midi,Client}),
                BC = whereis(midi_hub),
                _Ref = erlang:monitor(process, BC),
                handle(restart_port,
                       #{ open_port => OpenPort,
                          bc => BC })
        end,
        fun ?MODULE:handle/2})}.

%% See studio_sup:restart_port/1
handle(restart_port, State = #{open_port := Args}) ->
    case maps:find(port, State) of
        {ok, Port} ->
            Port ! {self(), {command, <<>>}},
            receive {Port, {exit_status, _}}=_E -> ok
            after 3000 -> exit({restart_port_timeout, Args})
            end;
        _ ->
            ok
    end,
    tools:info("start: ~p~n", [Args]),
    maps:put(port, apply(erlang,open_port,Args), State);
            

handle(exit, #{ port := Port } = State) ->
    Port ! {self(), {command, <<>>}},
    State;

handle({Port,{exit_status,_}=E}, #{ port := Port } = _State) ->
    exit(E);


%% Midi in. Translate to symbolic form.
%%
%% The time stamp is the jack frame number modulo 256.  It should be
%% enough to recover from any jitter that is encountered between the
%% jack midi receive and the writing to disk, since midi and audio
%% take different paths.

handle({Port,{data, Msg}}, 
       #{ port := Port, bc := BC } = State) ->
    %% log:info("~p~n",[Msg]),
    case maps:find(recorder, State) of
        {ok, Pid} -> Pid ! Msg;
        _ -> ok
    end,
    case Msg of
        <<31,_,16#F0,_/binary>>=_Sysex ->
            %% Note that MIDI spec allows real-time messages to be
            %% mixed with sysex messages, but what comes from
            %% jack_midi.c will be clean sysex.
            %% log:info("sysex ~p~n", [size(_Sysex)]),
            ok;
        <<MidiPort,TimeStamp,BinMidi/binary>> ->
            lists:foreach(
              fun(DecMidi) -> BC ! {broadcast, {midi, TimeStamp, {jack, MidiPort}, DecMidi}} end,
              midi:decode(BinMidi))
    end,
    State;

%% The recorder is separate from the broadcast mechanism.  Assume there is only one.
handle({record, _}, State = #{ recorder := Pid }) ->
    log:info("WARNING: already recording to ~p.~n", [Pid]), State;

handle({record, Pid}, State) ->
    _Ref = erlang:monitor(process, Pid),
    maps:put(recorder, Pid, State);

handle({Port,_Msg={data,Data}}, #{ port := Port } = State) ->
    log:info("bad proto: ~p~n",[Data]),
    State;

%% Midi out

%% jack_midi ! {midi,16#80000000,<<16#F0, 16#60, 63, 1,2,3,4,5,6,7, 16#F7>>}.


%% Control messages are encapsulated as sysex
handle({control,Bin}, State) ->
    %% Encode and wrap.
    Enc = midi:sysex_encode(Bin),
    log:info("sysex enc: ~p~n", [Enc]),
    Midi = iolist_to_binary([16#F0, 16#60, Enc, 16#F7]),
    log:info("sysex midi: ~p~n", [{Bin,Midi}]),
    handle({midi, 16#80000000, Midi}, State);
%% jack_midi ! {edit, #{ type=>0,track=>0,phase=>0,port=>0,midi=>midi:encode({cc,0,1,2})}}.
handle({edit,
        #{ type := Type, track := Track, phase := Phase, port := Port,
           midi := Midi }},
       State) when is_binary(Midi) ->
    NbBytes = size(Midi),
    PaddedMidi = binary:part(<<Midi/binary, 0,0,0,0>>, 0, 4),
    %% Map it to C struct layout.
    handle({control,
            <<1:32/little,  %% cmd
              Type,
              Track,
              0:16/little,
              Phase:16/little,
              NbBytes,
              Port,
              PaddedMidi/binary>>},
           State);

handle({midi,PortMask,Data}, #{ port := Port } = State) ->
    Bin = ?IF(is_binary(Data), Data, midi:encode(Data)),
    Port ! {self(), {command, <<PortMask:32/little, Bin/binary>>}},
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).



%% jack_midi ! {midi,16#80000000,<<16#F0, 16#60, 63, 1,2,3,4,5,6,7, 16#F7>>}.

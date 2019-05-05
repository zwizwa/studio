%% Jack midi interface.

%% While this is much better than alsa_midi, please note that Erlang
%% is too jittery to have really good timing.  It is best to treat
%% midi processing code as "datapath" code, and use the Erlang bridge
%% only for convenience.

-module(jack_midi).
-export([start_link/4, handle/2, balance/2]).

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
                          dump => idle,
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
       #{ port := Port, bc := BC, dump := Dump } = State) ->
    %% log:info("~p~n",[Msg]),
    case maps:find(recorder, State) of
        {ok, Pid} -> Pid ! Msg;
        _ -> ok
    end,
    case Msg of
        %% Sysex dump to the control port is assumed to be
        %% s-expressions.  Message is chunked since we can't transfer
        %% too much at once without causing real-time issues.
        <<31,_TimeStamp,16#F0,_/binary>>=Sysex ->
            {Balance0,Chunks0,ReplyTo} = Dump,  %% Just fail on bad match.
            N = size(Sysex),
            Chunk = binary:part(Sysex, 4, N-5),
            Balance = balance(Balance0, Chunk),
            Chunks = [Chunk|Chunks0],
            case Balance of
                0 ->
                    %% Form is complete.
                    Bin = iolist_to_binary(lists:reverse(Chunks)),
                    Term = type:decode({pterm, Bin}),
                    %% log:info("pterm:~p~n", [Term]),
                    case ReplyTo of
                        no_reply -> ok;
                        _ -> obj:reply(ReplyTo, {ok, Term})
                    end,
                    maps:put(dump, idle, State);
                _ ->
                    maps:put(dump, {Balance,Chunks,ReplyTo}, State)
            end;
        <<MidiPort,TimeStamp,BinMidi/binary>> ->
            lists:foreach(
              fun(DecMidi) -> 
                      BC ! {broadcast,
                            {midi, TimeStamp,
                             {jack, MidiPort}, DecMidi}} end,
              midi:decode(BinMidi)),
            State
    end;

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


%% Control messages are encapsulated as sysex.  This allows
%% generalization later to MIDI-only links.

handle({control,Bin}, State) ->
    %% Encode and wrap.
    Enc = midi:sysex_encode(Bin),
    Midi = iolist_to_binary([16#F0, 16#60, Enc, 16#F7]),
    %% log:info("sysex midi: ~p~n", [{Bin,Midi}]),
    handle({midi, 16#80000000, Midi}, State);
%% jack_midi ! {edit, #{ type=>0,track=>0,phase=>0,port=>0,midi=>midi:encode({cc,0,1,2})}}.
handle({edit,
        #{ type := Type, track := Track, phase := Phase, port := Port,
           midi := Midi }},
       State) when is_binary(Midi) ->
    NbBytes = size(Midi),
    PaddedMidi = binary:part(<<Midi/binary, 0,0,0,0>>, 0, 4),
    %% Map it to C struct layout from jack_midi.c
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

handle({clear_track, N}, State) when is_number(N) ->
    handle({control,
            <<2:32/little,  %% cmd
              N:32/little>>},
           State);

%% FIXME: Dumps should be serialized via another process.  For
%% convenience we do implement obj:call interface here, but will need
%% to return an error if an operation is in progress.
handle({ReplyTo,{dump_track, N}}, State = #{ dump := Dump }) when is_number(N) ->
    case Dump of
        idle ->
            handle({control,
                    <<3:32/little,  %% cmd
                      N:32/little>>},
                   maps:put(dump,{0,[],ReplyTo}, State));
        _ ->
            obj:reply(ReplyTo, {error, busy})
    end;

handle({midi,PortMask,Data}, #{ port := Port } = State) ->
    Bin = ?IF(is_binary(Data), Data, midi:encode(Data)),
    Port ! {self(), {command, <<PortMask:32/little, Bin/binary>>}},
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).



%% jack_midi ! {midi,16#80000000,<<16#F0, 16#60, 63, 1,2,3,4,5,6,7, 16#F7>>}.

%% Compute expression balance.
balance(S0, Bin) ->
    lists:foldr(
      fun($[,S) -> S+1;
         ($],S) -> S-1;
         (_, S) -> S
      end,
      S0,
      binary_to_list(Bin)).

%% {0,<<"[t,1500,[e,0,0,[m,176,1,2]],[e,0,0,[m,176,1,2]],[e,0,0,">>}
%% {0,<<"[m,176,1,2]],[e,0,0,[m,176,1,2]]]">>}

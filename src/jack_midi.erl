%% Jack midi interface.

%% While this is much better than alsa_midi, please note that Erlang
%% is too jittery to have really good timing.  It is best to treat
%% midi processing code as "datapath" code, and use the Erlang bridge
%% only for convenience.

%% TODO:
%%
%% - Since jackd already has a fast internal connection mechanism,
%%   maybe it is best to annotate midi messages with the port alias
%%   name instead of a number, to avoid a level of indirection?  Or
%%   maybe map those names to symbols?



-module(jack_midi).
-export([start_link/1, handle/2
        %% ,balance/2
        ]).

-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).

%% JACK midi client. Preferred as it has better timing properties.
%% Supports midi in/out, clock generation, and jack client connection.
-define(JACK_MIDI_CMD_MIDI,0).
-define(JACK_MIDI_CMD_CONNECT,1).

start_link(#{ client := Client,
              hubs   := NeedHubs,
              midi_ni := MidiNI,
              midi_no := MidiNO,
              clock_mask := ClockMask}) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name(jack_midi),
                register(jack_midi, self()),  %% FIXME: is this necessary?
                Cmd = 
                    tools:format(
                      "~s jack_midi ~s ~p ~p ~p",
                      [jack_daemon:studio_elf(),
                       Client, MidiNI, MidiNO, ClockMask]),
                OpenPort =
                    [{spawn,Cmd},
                     [{packet,4}
                     ,binary,exit_status]],

                log:set_info_name({jack_midi,Client}),

                BCS = NeedHubs(midi_hub),
                [erlang:monitor(process, BC) || BC <- BCS],
                handle(restart_port,
                       #{ open_port => OpenPort,
                          dump => idle,
                          clock_mask => ClockMask,
                          bcs => BCS })
        end,
        fun ?MODULE:handle/2})}.

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
       #{ port := Port, bcs := BCS, dump := Dump } = State) ->
    %% log:info("~p~n",[Msg]),
    case maps:find(recorder, State) of
        {ok, Pid} -> Pid ! Msg;
        _ -> ok
    end,
    case Msg of
        %% Sysex dump to the control port is assumed to be
        %% s-expressions.  Message is chunked since we can't transfer
        %% too much at once without causing real-time issues.  FIXME:
        %% What's a good way to tag this? A reserved manufacturer
        %% field is used.
        <<31,_TimeStamp, 16#F0, 16#60, Type, _/binary>>=Sysex ->
            %% This reply must have come from a request, which will
            %% set the dump state.  Just fail on bad match.
            {Chunks0,ReplyTo} = Dump,  
            N = size(Sysex),
            Chunk = binary:part(Sysex, 5, N-6),
            Chunks = [Chunk|Chunks0],
            case Type of
                1 -> 
                    %% log:info("pterm: incomplete~n"),
                    maps:put(dump, {Chunks, ReplyTo}, State);
                0 ->
                    Bin = iolist_to_binary(lists:reverse(Chunks)),
                    Term = type:decode({pterm, Bin}),
                    %% log:info("pterm: ~p~n", [Term]),
                    obj:reply(ReplyTo, {ok, Term}),
                    maps:put(dump, idle, State);
                _ ->
                    log:info("bad sysex type ~p~n",[Type]),
                    State
            end;
        <<31,_/binary>> ->
            log:info("bad control port message ~p~n",[Msg]),
            State;
        <<MidiPort,TimeStamp,BinMidi/binary>> ->
            Node = node(),
            Midi = midi:decode(BinMidi),
            PortTag = {port,in,MidiPort},
            lists:foreach(
              fun(DecMidi) ->
                      %% 1. Broadcasters.
                      lists:foreach(
                        fun(BC) ->
                                BC ! {broadcast,
                                      {midi, TimeStamp,
                                       {jack, Node, MidiPort}, DecMidi}}
                        end,
                        BCS),
                      %% 2. Epids
                      %% All midi output
                      epid:dispatch(PortTag, DecMidi, State),
                      %% Fine grained epids
                      case DecMidi of
                          {cc,Chan,CC,Val} ->
                              FilterTag = {filter,PortTag,{cc,Chan,CC}},
                              %% log:info("decmidi ~p -> ~p~n", [DecMidi,FilterTag]),
                              epid:dispatch(FilterTag, Val, State);
                          {on,Chan,Note,Vel} ->
                              FilterTag = {filter,PortTag,{on,Chan,Note}},
                              epid:dispatch(FilterTag, Vel, State);
                          _ ->
                              ok
                      end
                          
              end,
              Midi),
            State
    end;

%% epid:subscribe installs a monitor. pass on the message.
handle({'DOWN',_Ref,process,_Pid,_Reason}=Msg, State) ->
    epid:down(Msg, State);


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

handle({track_period, N, T}, State) when is_number(N) ->
    handle({control,
            <<4:32/little,  %% cmd
              N:32/little,
              T:32/little>>},
           State);

%% FIXME: Dumps should be serialized through another process.  For
%% convenience we do implement the obj:call interface here, but will
%% need to return an error if an operation is in progress.
handle({ReplyTo,{dump_track, N}}, State = #{ dump := Dump }) when is_number(N) ->
    case Dump of
        idle ->
            handle({control,
                    <<3:32/little,  %% cmd
                      N:32/little>>},
                   maps:put(dump,{[],ReplyTo}, State));
        _ ->
            obj:reply(ReplyTo, {error, busy})
    end;

handle({midi,PortMask,Data}, #{ port := Port } = State) ->
    Bin = ?IF(is_binary(Data), Data, midi:encode(Data)),
    Port ! {self(), {command, <<PortMask:32/little, Bin/binary>>}},
    State;

%% Epid interface.
handle({epid_send,Epid,Msg}=EpidSend, State) ->
    log:info("jack_midi:handle: ~p~n", [EpidSend]),
    case Msg of
        %% Internal connections.  FIXME: Check epids here?  Otherwise
        %% it is implicit, e.g. only what dispatch implements.
        {epid_subscribe, DstEpid} ->
            epid:subscribe(Epid, DstEpid, State);
        {epid_unsubscribe, DstEpid} ->
            epid:unsubscribe(Epid, DstEpid, State);
        _ ->
            log:info("Bad epid command: ~p~n", [EpidSend]),
            State
    end;

handle(Msg, State) ->
    obj:handle(Msg, State).



%% jack_midi ! {midi,16#80000000,<<16#F0, 16#60, 63, 1,2,3,4,5,6,7, 16#F7>>}.

%% %% Compute expression balance.
%% balance(S0, Bin) ->
%%     lists:foldr(
%%       fun($[,S) -> S+1;
%%          ($],S) -> S-1;
%%          (_, S) -> S
%%       end,
%%       S0,
%%       binary_to_list(Bin)).

%% {0,<<"[t,1500,[e,0,0,[m,176,1,2]],[e,0,0,[m,176,1,2]],[e,0,0,">>}
%% {0,<<"[m,176,1,2]],[e,0,0,[m,176,1,2]]]">>}

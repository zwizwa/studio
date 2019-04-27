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
       fun ?MODULE:handle/2}).

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
    <<MidiPort,TimeStamp,BinMidi/binary>> = Msg,
    lists:foreach(
      fun(DecMidi) -> BC ! {broadcast, {midi, TimeStamp, {jack, MidiPort}, DecMidi}} end,
      midi:decode(BinMidi)),
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
handle({midi,Mask,Data}, #{ port := Port } = State) ->
    Bin = ?IF(is_binary(Data), Data, midi:encode(Data)),
    Port ! {self(), {command, <<Mask:32/little, Bin/binary>>}},
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).


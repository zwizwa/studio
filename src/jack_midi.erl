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

start_link(Client,NI,NO,ClockMask) ->
    serv:start(
      {handler,
       fun() ->
               Cmd = 
                   tools:format(
                     "~s jack_midi ~s ~p ~p ~p",
                     [studio_sup:studio_elf(), Client, NI, NO, ClockMask]),
               OpenPort =
                   [{spawn,Cmd},[{packet,1},binary,exit_status]],
               log:set_info_name({jack_midi,Client}),
               handle(restart_port, #{ open_port => OpenPort })
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
handle({Port,{data,<<MidiPort,Data/binary>>}}, #{ port := Port } = State) ->
    case whereis(midi_hub) of
        undefined ->
            ok;
        MidiHub ->
            lists:foreach(
              fun(Msg) -> serv:hub_send(MidiHub, {{jack,MidiPort},Msg}) end,
              midi:decode(Data))
    end,
    State;

%% Midi out
handle({midi,Mask,Data}, #{ port := Port } = State) ->
    Bin = ?IF(is_binary(Data), Data, midi:encode(Data)),
    Port ! {self(), {command, <<Mask:32/little, Bin/binary>>}},
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).

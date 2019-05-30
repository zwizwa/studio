%% OBSOLETE.  Use jack_midi instead, which has much better timing.

-module(alsa_midi).
-export([alsa_seq_in/1, alsa_seq_in/2, alsa_seq_handle/2]).


%% Alsa sequencer client, midi in.
%% Only a single listener is necessary: distinguish based on source addr.
%% E.g. use aconnect in udev?
alsa_open(Client) ->
    open_port({spawn, jack_daemon:studio_elf() ++ " alsa_seq_in " ++ Client},
              [{packet,1},binary,exit_status]).
alsa_seq_in(Client, Sink) ->
    serv:start({handler,
                fun() -> {alsa_open(Client), Sink} end,
                fun midi:alsa_seq_handle/2}).
alsa_seq_in(Client) ->
    alsa_seq_in(Client,
                fun(Msg) -> tools:info("~p~n",[Msg]) end).
alsa_seq_handle({Port,{data,Data}},{Port,Sink}=State) ->
    alsa_seq_route(Data, Sink), State.
alsa_seq_route(<<Type,_Flags,_Tag,_Queue,
                 _TimeStamp:64/little,
                 SC,SP,_DC,_DP,
                 Data/binary>>, Sink) ->
    alsa_seq_ev(Type,{seq,{SC,SP}},Data,Sink).
alsa_seq_ev(6, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{on, C,K,V}});
alsa_seq_ev(7, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{off,C,K,V}});
alsa_seq_ev(10,S,<<C,_,_,_,K:32/little,V:32/signed-little,_/binary>>,Sink) -> Sink({S,{cc, C,K,V}});
alsa_seq_ev(Type,S,Data,Sink) -> Sink({S,{Type,Data}}).
    


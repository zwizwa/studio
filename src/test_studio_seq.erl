-module(test_studio_seq).
-compile([export_all]).

seq() ->
    %% Sequence as recorded by hub.c / jack_client.erl
    [{19520,{on,0,60,4}},
     {28544,{off,0,60,41}},
     {35200,{on,0,66,48}},
     {40896,{off,0,66,28}},
     {49728,{on,0,66,28}},
     {56640,{off,0,66,7}},
     {59520,{on,0,60,35}},
     {66048,{off,0,60,31}},
     {74240,{on,0,66,24}},
     {81664,{off,0,66,18}},
     {88576,{on,0,66,48}},
     {94912,{off,0,66,71}}].

%% Sequences are [{Timestamp, Stuff}].
%% Normalize to T=0, average timestamp, pick first payload.
split_loop(Seq) ->
    N = length(Seq),
    {F,S} = lists:split(N div 2, Seq),
    [{TF,_}|_] = F,
    [{TS,_}|_] = S,
    Len = TS - TF,
    {Len,
     lists:zipwith(
       fun({F,Stuff},{S,_DropStuff}) -> {(F+S) div 2, Stuff} end,
       F1 = time_shift(F),
       S1 = time_shift(S))}.

%% Convert to pattern sequencer form: each entry contains the delay to
%% the next entry, which is simpler to implement.  Pattern needs to
%% start at 0 for this to work.
pattern({Len,Seq=[{0,_}|_]}) ->
    lists:zipwith(
      fun({T,Stuff},{Tnext,_}) ->
              {{event,Stuff},{delay,Tnext-T}}
      end,
      Seq,
      tl(Seq) ++ [{Len,loop}]).

%% Shift the time tags to T=0 for first event.
time_shift(Lst=[{T0,_}|_]) ->
    [{T-T0,Stuff} || {T,Stuff}<-Lst].

%% Change the time scale from audio samples to midi clock ticks.
time_scale(NbClocks, Seq={Len, Evts}) ->
    %% 1. Given the number of logical ticks we want (24 per quarter
    %%    note), find the time div.  Use floating point to compute
    %%    this to get better resolution for the individual timestamps.
    SamplesPerClock = round(Len / NbClocks),

    %% 2. Scale each timestamp and round to grid.
    Evts1 = [{round(T / SamplesPerClock), Stuff} || {T,Stuff} <- Evts],
    {SamplesPerClock, {NbClocks, Evts1}}.

%% FIXME: The idea is to always represent tempo as an integer multiple
%% of the sample frequency.  Maybe even a double, to allow to derive a
%% square wave as well.  What effect does that have on the clocks?

split_loop() ->
    Seq = [{T,stuff} || T <- [0,1,2,4,5,6]],
    split_loop(Seq).


t() ->
    Seq = split_loop(seq()),
    SeqClock = time_scale(24 * 2, Seq),
    {_Scale, Seq1} = SeqClock,
    #{samples => Seq,
      midi_clock => SeqClock,
      pattern => pattern(Seq1)}.


    
           

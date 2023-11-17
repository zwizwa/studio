-module(test_studio_seq).
-compile([export_all]).

seq() ->
    %% Better to work with numerical codes from the start, since
    %% everything ends up as a tag_u32 call.
    On = 9,
    Off = 8,
    %% Sequence as recorded by hub.c / jack_client.erl
    [{19520,{On,0,60,4}},
     {28544,{Off,0,60,41}},
     {35200,{On,0,66,48}},
     {40896,{Off,0,66,28}},
     {49728,{On,0,66,28}},
     {56640,{Off,0,66,7}},
     {59520,{On,0,60,35}},
     {66048,{Off,0,60,31}},
     {74240,{On,0,66,24}},
     {81664,{Off,0,66,18}},
     {88576,{On,0,66,48}},
     {94912,{Off,0,66,71}}].

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

%% Convert to pattern sequencer commands, setting global tempo,
%% clearing pattern and adding steps.  Each step is an event and the
%% delay to the next event.  Pattern needs to start at 0 for this to
%% work.
pattern(PatNb, {ClockDiv, {Len,Seq=[{0,_}|_]}}) ->
    [[clock_div, ClockDiv],
     [pat_clear, PatNb]] ++
    lists:zipwith(
      fun({T,Stuff},{Tnext,_}) ->
              {Type, Track, Arg1, Arg2} = Stuff,
              Delay = Tnext - T,
              [pat_add, PatNb, Type, Track, Arg1, Arg2, Delay] 
      end,
      Seq,
      tl(Seq) ++ [{Len, sentinel_ignored}]).

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
    #{samples => Seq,
      midi_clock => SeqClock,
      pattern => pattern(0, SeqClock)}.


    
           

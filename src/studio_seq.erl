-module(studio_seq).
-export([split_loop/1, time_scale/2, pattern/1]).

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
pattern({ClockDiv, {Len,Seq=[{0,_}|_]}}) ->
    fun(PatNb) ->
            [[clock_div, ClockDiv]] ++
            lists:zipwith(
              fun({T,Stuff},{Tnext,_}) ->
                      {Type, Track, Arg1, Arg2} = Stuff,
                      Delay = Tnext - T,
                      [pat_add, PatNb, Type, Track, Arg1, Arg2, Delay] 
              end,
              Seq,
              tl(Seq) ++ [{Len, sentinel_ignored}])
    end.

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

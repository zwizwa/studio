-module(studio_seq).
-export([split_loop/1,
         time_scale/2,
         pattern/1,
         save_patterns/1,
         save_pattern/2,
         load_pattern/2,
         set_clock_div/2,
         pattern_unpack/1,
         save/1
        ]).

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

%% FIXME: Change this to binary set_raw_steps and remove the
%% pattern_begin, pattern_end, step commands.

pattern({ClockDiv, {Len,Seq=[{0,_}|_]}}) ->
    [[clock_div, ClockDiv],
     [pattern_begin]] ++
    lists:zipwith(
      fun({T,Stuff},{Tnext,_}) when is_binary(Stuff) ->
              Delay = Tnext - T,
              {[step, Delay], Stuff} 
      end,
      Seq,
      tl(Seq) ++ [{Len, sentinel_ignored}]) ++
    [[pattern_end]].


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



%% The API uses a binary blob for the sequencer pattern data.  Simple
%% enough to encode arrays this way, so seems best to avoid multiple
%% calls.
pattern_unpack(Bin) ->
    [{{A,B,C,D},Delay} || <<A,B,C,D,Delay:16/little>> <= Bin].
   

%% Queries

%% i:hub()
save_patterns(HubPid) ->
    {[0], Bin} = tag_u32:call(HubPid, [save_patterns]),
    [Nb || <<Nb:16/little>> <= Bin].
save_pattern(HubPid, Pattern) ->
    case tag_u32:call(HubPid, [save_pattern,Pattern]) of
        {[0], Bin} ->
            {ok, Bin};
        _ ->
            error
    end.
load_pattern(HubPid, Bin) when is_binary(Bin) ->
    {[0, PatNb], <<>>} = tag_u32:call(HubPid, [load_pattern], Bin),
    PatNb.

save(HubPid) ->
    Patterns = save_patterns(HubPid),
    [begin
         {ok, Steps} = save_pattern(HubPid, Pattern),
         Steps
     end || Pattern <- Patterns].
    

%% i:clock()
set_clock_div(ClockPid, Div) ->        
    tag_u32:call(ClockPid, [clock_div, Div]).

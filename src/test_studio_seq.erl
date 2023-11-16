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
       F1 = rebase0(F),
       S1 = rebase0(S))}.

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

%% Rebase time scale to T=0 for first event.
rebase0(Lst=[{T0,_}|_]) ->
    [{T-T0,Stuff} || {T,Stuff}<-Lst].

split_loop() ->
    Seq = [{T,stuff} || T <- [0,1,2,4,5,6]],
    split_loop(Seq).

t() ->
    Seq = split_loop(seq()),
    #{seq => Seq, pattern => pattern(Seq)}.


    
           

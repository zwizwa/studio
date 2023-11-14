-module(test_studio_seq).
-compile([export_all]).

record1() ->
    %% Sequence ending on the loop note.
    Seq = [start,
           {on,21248,0,36,19},
           {off,29312,0,36,56},
           {on,36480,0,37,22},
           {off,41792,0,37,8},
           {on,51072,0,42,40},
           {off,56448,0,42,36},
           {on,62976,0,36,61},
           {off,68864,0,36,61},
           stop],
    %% Processing steps:
    %% 1. Rebase time.
    Seq.

%% Do the simple case first: focus on the timestamps.
simple() ->
    Seq = [0,1,2,4,5,6],
    {F,S} = lists:split(3, Seq),
    [TF|_] = F,
    [TS|_] = S,
    Len = TS - TF,
    F1 = normalize(F),
    S1 = normalize(S),
    lists:zipwith(
      fun(F,S) -> (F+S)/2.0 end,
      F1, S1).
normalize(Lst=[T0|_]) ->
    [T-T0 || T<-Lst].



    
           

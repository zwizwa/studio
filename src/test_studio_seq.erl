-module(test_studio_seq).
-compile([export_all]).

seq() ->
    [{on,21248,0,36,19},
     {off,29312,0,36,56},
     {on,36480,0,37,22},
     {off,41792,0,37,8},
     {on,51072,0,42,40},
     {off,56448,0,42,36},
     {on,62976,0,36,61},
     {off,68864,0,36,61}
    ].
stuff(Seq) ->
    [{Time div 64,{Track,Evt,Note,Vel}}
     || {Evt,Time,Track,Note,Vel} <- Seq].

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
    Seq = split_loop(stuff(seq())),
    #{seq => Seq, pattern => pattern(Seq)}.


    
           

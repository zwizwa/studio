-module(test_studio_seq).
-compile([export_all]).

seq() ->
    %% Sequence as recorded by hub.c / jack_client.erl
    [{19520, <<0, 16#90, 60,  4>>},
     {28544, <<0, 16#80, 60, 41>>},
     {35200, <<0, 16#90, 66, 48>>},
     {40896, <<0, 16#80, 66, 28>>},
     {49728, <<0, 16#90, 66, 28>>},
     {56640, <<0, 16#80, 66,  7>>},
     {59520, <<0, 16#90, 60, 35>>},
     {66048, <<0, 16#80, 60, 31>>},
     {74240, <<0, 16#90, 66, 24>>},
     {81664, <<0, 16#80, 66, 18>>},
     {88576, <<0, 16#90, 66, 48>>},
     {94912, <<0, 16#80, 66, 71>>}].

bin_patterns() ->
    [<<0,144,60,29,4,0,0,144,66,71,2,0,0,128,60,44,4,0,0,128,
       66,33,12,0,0,144,66,26,5,0,0,128,66,28,10,0,0,144,66,
       24,6,0,0,128,66,28,5,0>>,
     <<255,0,0,0,12,0,0,144,72,84,7,0,0,128,72,35,29,0>>].

t() ->
    Seq = studio_seq:split_loop(seq()),
    SeqClock = studio_seq:time_scale(24 * 2, Seq),
    {_,LenSeq} = SeqClock,
    #{samples => Seq,
      midi_clock => SeqClock,
      pattern => studio_seq:pattern_pack(LenSeq)}.


%% Run against jack_client hub.c
t1(HubPid) ->
    #{ pattern := StepsBin } = t(),
    studio_seq:load_pattern(HubPid, StepsBin).
    
t2(HubPid) ->
    [studio_seq:load_pattern(HubPid, StepsBin)
     || StepsBin <- bin_patterns()].


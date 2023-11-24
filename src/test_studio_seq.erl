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

t() ->
    Seq = studio_seq:split_loop(seq()),
    SeqClock = studio_seq:time_scale(24 * 2, Seq),
    #{samples => Seq,
      midi_clock => SeqClock,
      pattern => studio_seq:pattern(SeqClock)}.


%% Run against jack_client hub.c
t(Pid) ->
    Program =
      [[clock_div,833],
       [pattern_begin],
       [step,9,0,60,4,9],
       [step,8,0,60,41,9],
       [step,9,0,66,48,8],
       [step,8,0,66,28,10],
       [step,9,0,66,28,8],
       [step,8,0,66,7,4],
       [pattern_end]],
    jack_client:program(Pid, Program).

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
       {[step,  9], <<0, 16#90, 60,  4>>},
       {[step,  9], <<0, 16#80, 60, 41>>},
       {[step,  8], <<0, 16#90, 66, 48>>},
       {[step, 10], <<0, 16#80, 66, 28>>},
       {[step,  8], <<0, 16#90, 66, 28>>},
       {[step,  4], <<0, 16#80, 66,  7>>},
       [pattern_end]],
    jack_client:program(Pid, Program).

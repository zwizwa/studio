-module(jack_record).
-export([start_link/0]).

start_link() ->
    {ok, Pid} =
        recorder:start_link(
          #{ dir => "/vol/studio/",
             nb_chunks => 450, %% FIXME: make this depend on disk size
             chunk_size => 1000*1000*1000 }),
    %% Subscribe midi an audio.
    midi_hub ! {subscribe, Pid},
    {ok, Pid}.

-module(jack_record).
-export([start_link/1]).

start_link(Sources) ->
    {ok, Pid} =
        recorder:start_link(
          #{ dir => "/vol/studio/",
             nb_chunks => 450, %% FIXME: make this depend on disk size
             chunk_size => 1000*1000*1000 }),
    lists:foreach(
      fun(Source) -> Source ! {record, Pid} end,
      Sources),
    {ok, Pid}.

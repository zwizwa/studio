-module(jack_record).
-export([start_link/1]).

%% If this is a mount point, disksup can be used.
%% -define(DIR,"/vol/34e6aa85-8440-4d19-ac76-5646a856443e/").
-define(DIR,"/vol/studio/").

-define(GB, 1000000000).

-define(MB, 1000000).

start_link(Sources) ->
    {ok, Pid} =
        recorder:start_link(
          #{ dir => ?DIR,
             usage => {nb_bytes, 450 * ?GB}, chunk_size => 1 * ?GB
             %% usage => {nb_bytes, 100 * ?MB}, chunk_size => 10 * ?MB
           }),
    register(jack_record, Pid),
    lists:foreach(
      fun(Source) -> Source ! {record, Pid} end,
      Sources),
    {ok, Pid}.

%% jack_record:start_link([jack_audio,jack_midi]).

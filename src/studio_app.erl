-module(studio_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _Args) ->
    %% All processes are supervised.
    studio_sup:start_link().

stop(_State) ->
    ok.

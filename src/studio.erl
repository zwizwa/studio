-module(studio).
-export([start/0]).

start() ->
    serv:up(jackd,{spawner, fun midi:jackd/0}).

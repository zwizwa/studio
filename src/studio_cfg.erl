%% Indirection for studio_db.

%% For exo this is overridden: if these functions exist they are used:
%%  exo:midi_port_id/1
%%  exo:midi_clock_ports/0

-module(studio_cfg).
-export([port_pair/1, port_id/1, midiclock_mask/0]).

port_pair(CP) ->
    studio_db:port_pair(CP).

%% Map name as detected by jack to id number used in exo.  The default
%% is to use the studio_db, but exo can override.

port_id(Name) ->
    case erlang:function_exported(exo, midi_port_id, 1) of
        true -> exo:midi_port_id(Name);
        false -> studio_db:port_id(Name)
    end.

midiclock_mask() ->
    case erlang:function_exported(exo, midi_clock_ports, 0) of
        true ->
            lists:foldl(
              fun(Name, Acc) ->
                      PortId = port_id(Name),
                      Acc + (1 bsl PortId)
              end,
              0, exo:midi_clock_ports());
        false ->
            studio_db:midiclock_mask()
    end.

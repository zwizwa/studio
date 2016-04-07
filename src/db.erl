-module(db).
-export([start_link/0,
         port_id/1,
         midiclock_mask/0
        ]).

start_link() ->
    Pid = sqlite3:open("priv/db.sqlite3"),
    register(db,Pid),
    {ok, Pid}.

port_id(Name) when is_binary(Name) ->
    case sqlite3:query(db,select,
                       [[port_id], midiport,
                        {where, {eq, port_name, Name}}]) of
        [[PortId]] ->
            binary_to_integer(PortId);
        _ ->
            0
    end.

midiclock_mask() ->
    binary_to_integer(
      hd(hd(
           sqlite3:query(
             db,select,[midiclock_mask,all])))).

-module(studio_db).
-export([connect/2, disconnect/2, connections/0,
         midiclock_mask/0,
         port_id/1, port_pair/1,
         db/0, sql/1]).

db() -> 
    maps:merge(
      sqlite3:db_registered(
        db,
        fun db_file/0,
        fun db_init/1),
      %% when process is dead, call will fail. otherwise wait forever.
      %% 3 seconds seems quite normal, so only warn every 10 seconds.
      #{ timeout => {warn, 10000} }).

db_file() ->
    DbFile = code:priv_dir(studio) ++ "/db.sqlite3",
    log:info("db file = ~p~n", [DbFile]),
    DbFile.
db_init(_) ->
    ok.
sql(Queries) ->
    sqlite3:sql(db(), Queries).

midiclock_mask() ->
    [[[Mask]]] = sql([{<<"select * from midiclock_mask">>,[]}]),
    binary_to_integer(Mask).

port_pair({C,P}) when is_binary(C) and is_binary(P) ->
    {C,P};
port_pair(Str) ->
    [C,P] = binary:split(iolist_to_binary(Str),<<":">>,[global]),
    {C,P}.
    
connect(A,B) ->
    try
        {CA,PA} = port_pair(A),
        {CB,PB} = port_pair(B),
        Q = <<"insert or ignore into connect (client_a, port_a, client_b, port_b) values (?,?,?,?)">>,
        sql([{Q, [CA,PA,CB,PB]}])
    catch
        C:E ->
            log:info("WARNING: ~p~n",[{C,E}])
    end.

disconnect({CA,PA},{CB,PB}) when 
      is_binary(CA) and is_binary(PA) and
      is_binary(CB) and is_binary(PB) ->
    Q = <<"delete from connect where client_a=? and port_a=? and client_b=? and port_b=?">>,
    sql([{Q, [CA,PA,CB,PB]}]).

connections() ->
    [Table] = sql([{<<"select * from connect">>,[]}]),
    [{{CA,PA},{CB,PB}} || [CA,PA,CB,PB] <- Table].
  

port_id(Name) when is_binary(Name) ->
    case sql([{<<"select port_id from midiport where port_name = ?">>,[Name]}]) of
        [[[PortId]]] ->
            binary_to_integer(PortId);
        _ ->
            tools:info("WARNING: unknown port_id ~p~n",[Name]),
            0
    end.


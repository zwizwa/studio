-module(studio_db).
-export([connect/2, disconnect/2, connections/0,
         midiclock_mask/0,
         port_id/1, port_pair/1,
         db/0, sql/1]).

%% FIXME:  Currently hardcoded.  Change API such that this can be injected.
db() ->
    exo:db_local().
db_put(K,V) ->
    exo_db:put(K,V).
    

sql(Queries) ->
    sqlite3:sql(db(), Queries).

port_pair({C,P}) when is_binary(C) and is_binary(P) ->
    {C,P};
port_pair(Str) ->
    [C,P] = binary:split(iolist_to_binary(Str),<<":">>,[global]),
    {C,P}.
    
connected(A,B,Connected) ->
    try
        {CA,PA} = port_pair(A),
        {CB,PB} = port_pair(B),
        db_put([jackconn,CA,PA,CB,PB],[Connected])
    catch
        _C:_E ->
            log:info("WARNING: ~p~n",[{_C,_E}]),
            ok
    end.
connect(A,B)    -> connected(A,B,true).
disconnect(A,B) -> connected(A,B,false).   


connections() ->
    try
        [Table] = sql([{<<"select client_a,port_a,client_b,port_b from jackconn where connected = 'true' ">>,[]}]),
        [{{CA,PA},{CB,PB}} || [CA,PA,CB,PB] <- Table]
    catch _C:_E ->
            log:info("WARNING: ~p~n",[{_C,_E}]),
            []
    end.



%% See exo_db midiport table + exo_config
port_id(Name) when is_binary(Name) ->
    case sql([{<<"select port_id from midiport where port_name = ?">>,[Name]}]) of
        [[[PortId]]] ->
            binary_to_integer(PortId);
        _ ->
            tools:info("WARNING: unknown port_id ~p~n",[Name]),
            0
    end.

%% This used to be a view.  Don't do that as it spreads things out too
%% much over different systems.

%% CREATE VIEW midiclock_mask as
%% select sum(1<<port_id) from midiclock left join midiport on midiclock.port_name = midiport.port_name;
%% COMMIT;

midiclock_mask() ->
    %% [[[Mask]]] = sql([{<<"select * from midiclock_mask">>,[]}]),
    %% Note that exo_db can't represent things by _presence_ of
    %% records, so an explicit boolean "member" field is necessary
    %% that we use here to filter.
    [[[Mask]]] = 
        sql(
          [{<<"select sum(1<<port_id) "
              "from midiclock left join midiport "
              "on midiclock.port_name = midiport.port_name "
              "where midiclock.enable = 'true'">>,
            []}]),
    binary_to_integer(Mask).

%% FIXME: No longer used. Config is now hardcoded in studio_cfg.erl

-module(studio_db).
-export([midiclock_mask/0,
         port_id/1, port_pair/1,
         db/0, sql/1, tables/0, db_init/0]).

%% FIXME: Currently hardcoded.  Change API such that this can be injected.
%% FIXME: If the tables do not exist, create them during first contact.
db() ->
    exo:db_local().
tables() ->
    [{T,exo_db:local_table(T)} ||
        T <- [midiport,midiclock]].
        

%% exo_db:local_table(midiport).

%% Set up schema.  It would be simpler to do this with a command file
%% but it doesn't seem that the C api supports that.
db_init() ->
    [[],[],[]] = sql(
      [{<<"create table if not exists midiport ("
          "  port_id   INTEGER PRIMARY KEY NOT NULL,"
          "  port_name TEXT    NOT NULL"
          ");">>,[]},
       {<<"create table if not exists midiclock ("
          "  port_name  TEXT    PRIMARY KEY NOT NULL,",
          "  ts         INTEGER,"
          "  enable     TEXT",
          ");">>,[]},
       {<<"create view if not exists midiclock_mask as"
          "  select sum(1<<port_id) from midiclock left join midiport on midiclock.port_name = midiport.port_name;">>,[]}
      ]),
    ok.

sql(Queries) ->
    sqlite3:sql(db(), Queries).

port_pair({C,P}) when is_binary(C) and is_binary(P) ->
    {C,P};
port_pair(Str) ->
    case binary:split(iolist_to_binary(Str),<<":">>,[global]) of
        [C,P] -> {C,P};
        [C,P,P1] -> {C,<<P/binary, P1/binary>>} %% a2j ports have a colon
    end.
    

%% See exo_db midiport table + exo_config
port_id(Name) when is_binary(Name) ->
    case sql([{<<"select port_id from midiport where port_name = ?">>,
               [{text,Name}]}]) of
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
    case 
        sql(
          [{<<"select sum(1<<port_id) "
              "from midiclock left join midiport "
              "on midiclock.port_name = midiport.port_name "
              "where midiclock.enable = 'true'">>,
            []}]) of
        [[[<<>>]]] ->
            %% Why isn't this just <<"0">> when there are no ports enabled?
            0;
        [[[Mask]]] ->
            binary_to_integer(Mask)
    end.


        



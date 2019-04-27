%% Jack Daemon wrapper.  Note that this insists on managing the
%% daemon.  I.e. it is "framework-y".

-module(jack_daemon).
-export([start_link/0, handle/2]).


%% Wrap the daemon and listen on its stdout as a simple way to get
%% MIDI port connect notifications.

%% Once midi port aliases are known, connect them to a specified port
%% number on the jack client.

start_link() ->
    {ok, serv:start(
           {handler,
            fun() -> 
                    SH = code:priv_dir(studio) ++ "/start_jackd.sh",
                    tools:info("jackd_open: ~s~n",[SH]),
                    Port = open_port({spawn, SH},
                                      [{line,1024}, binary, use_stdio, exit_status]),
                    #{port => Port} end,
            fun ?MODULE:handle/2})}.

handle({Port, {data, {eol, Line}}}, #{port := Port} = State) ->
    handle({line, Line}, State);
handle({Port, {exit_status, _}=Msg}, #{port := Port}) ->
    exit(Msg);

handle({line, <<"scan: ", Rest/binary>>=_Line}, State) ->
    tools:info("~s~n",[_Line]),
    {match,[_|[Action,_,Dir,Addr,Name]]} =
        re:run(Rest,
               <<"(\\S+) port (\\S+) (\\S+)\\-(hw\\-\\d+\\-\\d+\\-\\d+)\\-(\\S+)\n*">>,
               [{capture,all,binary}]),
    PortAlias = <<Dir/binary,$-,Addr/binary,$-,Name/binary>>,
    Key = {Dir, Name},
    %%tools:info("~s ~p => ~p~n",[Action, Key, PortAlias]),
    case Action of
        <<"added">> ->
            S1=maps:put(Key,PortAlias,State),
            handle_connect(PortAlias, Dir, Name, S1);
        <<"deleted">> ->
            maps:remove(Key, State);
        _ ->
            State
    end;
handle({line, _Line}, State) -> 
    tools:info("~s~n",[_Line]),
    State;
handle({client, Msg}, State) ->
    case Msg of
        {_,tc} -> dont_print;
        _ -> tools:info("jack client message: ~p~n",[Msg])
    end,
    State;
handle(Msg, State) ->
    obj:handle(Msg, State).


handle_connect(PortAlias, Dir, Name, State) ->

    %% Client can be started only after daemon is up, so do it lazily.
    {C,State1} = control_client(State),

    N = integer_to_binary(port_id(Name)),
    %% tools:info("~p~n",[[PortAlias,Dir,Name,N]]),
    Connect =
        fun(Src,Dst) ->
                spawn(
                  fun() ->
                          %% Port creation seems to happen after it is
                          %% logged to the console.  Can't sync, so
                          %% add a workaround timeout.
                          timer:sleep(500),
                          obj:call(C,{connect,Src,Dst})
                  end)
        end,
                      
    case Dir of
        <<"in">>  -> Connect(PortAlias,<<"studio:midi_in_",N/binary>>);
        <<"out">> -> Connect(<<"studio:midi_out_",N/binary>>,PortAlias)
    end,
    State1.


control_client(State) ->
    State1 = need_clients(State),
    {maps:get(control, State1), State1}.

need_clients(State = #{ control := _, midi := _ }) -> 
    State;
need_clients(State) ->
    tools:info("starting clients~n"),
    ClockMask = midiclock_mask(),
    {ok, Control} = jack_control:start_link("studio_control"),
    {ok, Midi}    = jack_midi:start_link("studio",16,16,ClockMask),
    {ok, Audio}   = jack_audio:start_link("studio_audio", 8),
    maps:merge(
      State,
      #{control => Control,
        midi    => Midi,
        audio   => Audio
       }).


port_id(Name) when is_binary(Name) ->
    case sql([{<<"select port_id from midiport where port_name = ?">>,[Name]}]) of
        [[[PortId]]] ->
            binary_to_integer(PortId);
        _ ->
            tools:info("WARNING: unknown port_id ~p~n",[Name]),
            0
    end.



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

%% %% midiclock_mask() -> 16864.
%% midiclock_mask() ->
%%     binary_to_integer(
%%       hd(hd(
%%            sqlite3:query(
%%              db,select,[midiclock_mask,all])))).




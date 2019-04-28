%% Jack Daemon wrapper.  Note that this insists on managing the
%% daemon.  I.e. it is "framework-y".

%% FIXME: The output parsing can now be removed and replaced with
%% jack_control event-based functionality, but first, save the
%% contents of the databas.


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
    %% tools:info("~s~n",[_Line]),
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
    %% tools:info("~s~n",[_Line]),
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
    {Control, State1} = control_client(State),

    N = integer_to_binary(studio_db:port_id(Name)),
    %% tools:info("~p~n",[[PortAlias,Dir,Name,N]]),
    Connect =
        fun(Src,Dst) ->
                spawn(
                  fun() ->
                          %% Port creation seems to happen after it is
                          %% logged to the console.  Can't sync, so
                          %% add a workaround timeout.
                          timer:sleep(500),
                          Control ! {connect,Src,Dst}
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
    maps:merge(
      State,
      maps:from_list(
        [{Name,start_client(Name)} || Name <- [control, midi, audio]])).

start_client(Name) ->
    {ok, Pid} = 
        case Name of
            control -> jack_control:start_link("studio_control");
            midi    -> jack_midi:start_link("studio",16,16,studio_db:midiclock_mask());
            audio   -> jack_audio:start_link("studio_audio", 8)
        end,
    Pid.
                
                    




%% %% midiclock_mask() -> 16864.
%% midiclock_mask() ->
%%     binary_to_integer(
%%       hd(hd(
%%            sqlite3:query(
%%              db,select,[midiclock_mask,all])))).




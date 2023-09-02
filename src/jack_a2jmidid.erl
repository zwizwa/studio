%% wrapper for: a2jmidid -e

-module(jack_a2jmidid).
-export([start_link/1, handle/2]).

%% Start a processor or synth.
start_link(_) ->
    Init = #{},
    Pid = serv:start(
            {handler,
             fun() ->
                     log:set_info_name(?MODULE),
                     self() ! start,
                     Init
             end,
             fun ?MODULE:handle/2}),
    {ok, Pid}.

handle(start, State) ->
    SH = code:priv_dir(studio) ++ "/start_a2jmidid.sh",
    tools:info("aj2midid_open: ~s~n",[SH]),
    Opts = [{line,1024}, binary, use_stdio, exit_status],
    Port = open_port({spawn, SH}, Opts),
    maps:put(port, Port, State);

handle({Port, {data, Data}}, #{port := Port} = State) ->
    case Data of
        {eol, Line} ->
            log:info("~s~n", [Line]),
            handle({line, Line}, State);
        _ ->
            log:info("UNEXPECTED: Data=~p~n", [Data])
    end;
handle({Port, {exit_status, _}}=Msg, State = #{ port := Port }) ->
    log:info("WARNING: ~p~n", [Msg]),
    timer:send_after(2000, start),
    maps:remove(port, State);

%% FIXME: This example is copied from jack_daemon.erl
%% Do we need to parse the "port created:" etc messages?

%% handle({line, <<"scan: ", Rest/binary>>=_Line}, State) ->
%%     %% tools:info("~s~n",[_Line]),
%%     {match,[_|[Action,_HwAddr,Dir,Addr,Name]]} =
%%         re:run(
%%           Rest,
%%           <<"(\\S+) port (\\S+) (\\S+)\\-(hw\\-\\d+\\-\\d+\\-\\d+)\\-(\\S+)\n*">>,
%%           [{capture,all,binary}]),
%%     PortAlias = <<Dir/binary,$-,Addr/binary,$-,Name/binary>>,
%%     Key = {Dir, Name},
%%     %% log:info("~s ~p => ~p~n",[Action, Key, PortAlias]),
%%     case Action of
%%         <<"added">> ->
%%             %% S1=maps:put(Key,PortAlias,State),
%%             S1=State,
%%             handle_connect(PortAlias, Dir, Name, S1);
%%         <<"deleted">> ->
%%             maps:remove(Key, State);
%%         _ ->
%%             State
%%     end;

handle({line, Line}, State) -> 
    tools:info("~s~n",[Line]),
    State;

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).



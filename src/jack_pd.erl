%% Pd wrapper.  Connects to Jack.  If host has X session running it
%% will start Pd GUI.


-module(jack_pd).
-export([start_link/1, handle/2]).

start_link(Init = #{ spawn_port := _}) ->
    {ok, serv:start(
           {handler,
            fun() -> 
                    log:set_info_name(?MODULE),
                    %% timer:send_after(2000, start),
                    self() ! start,
                    Init
            end,
            fun ?MODULE:handle/2})}.

handle(start, State) ->
    SH = code:priv_dir(studio) ++ "/start_pd.sh",
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


handle({line, _Line}, State) -> 
    tools:info("~s~n",[_Line]),
    State;

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).



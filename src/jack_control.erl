-module(jack_control).
-export([start_link/1, handle/2]).

start_link(Client) ->
    Pid = 
        serv:start(
          {handler, 
           fun() ->
                   %% log:set_info_name({jack_control, Client}),
                   Cmd = tools:format("~s jack_control ~s", [studio_sup:studio_elf(), Client]),
                   Args = [{spawn,Cmd},[{packet,1},binary,exit_status]],
                   handle(restart_port, #{ open_port => Args })
           end,
           fun ?MODULE:handle/2}),
    register(jack_control, Pid),
    {ok, Pid}.

%% Jack control client
-define(JACK_CONTROL_CMD_CONNECT,1).

fmt_port(Bin) when is_binary(Bin) ->
    Bin;
fmt_port({C,P}) ->
    tools:format("~s:~s",[C,P]).

     

handle(restart_port, State = #{ open_port := Args }) ->
    case maps:find(port, State) of
        {ok, Port} ->
            Port ! {self(), {command, <<>>}},
            receive {Port, {exit_status, _}}=_E -> ok
            after 3000 -> exit({restart_port_timeout, Args})
            end;
        _ ->
            ok
    end,
    log:info("start: ~p~n", [Args]),
    maps:put(port, apply(erlang, open_port, Args), State);

%% Protocol is asynchronous.  This makes it easier to use the return
%% pipe for jack events.

handle({connect, Src0, Dst0} = _Msg, State = #{ port := Port }) ->
    log:info("~999p~n", [_Msg]),
    Src = fmt_port(Src0),
    Dst = fmt_port(Dst0),
    Cmd = <<?JACK_CONTROL_CMD_CONNECT,Src/binary,0,Dst/binary,0>>,
    Port ! {self(), {command, Cmd}},
    State;

handle({Port,{exit_status,_}=E}, _State = #{ port := Port }) ->
    exit(E);

handle({Port,{data, Data}}, State = #{ port := Port }) ->
    %% Protocol is a string embedded in {packet,1}, which is easy to
    %% generate in C and easy to massage here into nested data
    %% structures.
    Reg = fun(<<"1">>) -> true;
             (<<"0">>) -> false end,
    Parsed =
        case binary:split(Data,<<":">>,[global]) of
            [<<"client">>,R,C] -> {client, Reg(R), C};
            [<<"port">>,R,C,P] -> {port, Reg(R), {C, P}};
            [<<"connect">>,R,CA,PA,CB,PB] -> {connect, Reg(R), {CA,PA}, {CB,PB}};
            Split -> {error, Split}
        end,
    case Parsed of
        {connect, true,  A, B} -> studio_db:connect(A, B);
        %% FIXME: Distinguish between disconnect due to client exit
        %% and intentional, manual disconnect?
        %% {connect, false, A, B} -> studio_db:disconnect(A, B);
        _ -> ok
    end,
    log:info("~999p~n", [Parsed]),
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).



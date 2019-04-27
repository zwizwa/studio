-module(jack_control).
-export([start_link/1, handle/2]).

start_link(Client) ->
    {ok,
     serv:start(
       {handler, 
        fun() ->
                log:set_info_name({jack_control, Client}),
                Cmd = tools:format("~s jack_control ~s", [studio_sup:studio_elf(), Client]),
                Args = [{spawn,Cmd},[{packet,1},binary,exit_status]],
                handle(restart_port, #{ open_port => Args })
       end,
        fun ?MODULE:handle/2})}.

%% Jack control client
-define(JACK_CONTROL_CMD_CONNECT,1).

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

handle({Pid, _Msg={connect, Src, Dst}}, State = #{ port := Port }) ->
    log:info("~999p~n", [_Msg]),
    Reply = 
        rpc(
          <<?JACK_CONTROL_CMD_CONNECT,Src/binary,0,Dst/binary,0>>,
          Port),
    Pid ! {self(), obj_reply, Reply},
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).


rpc(Cmd, Port) ->
    Port ! {self(), {command, Cmd}},
    receive
        {Port,{exit_status,_}=E} -> exit(E);
        {Port,{data, Data}} -> Data
    end.


-module(jack_control).
-export([start_link/1, loop/1]).

start_link(Client) ->
    serv:start({body, 
                fun() -> ?MODULE:loop(
                           #{port => jack_control_open(Client)})
                end}).

%% Jack control client
-define(JACK_CONTROL_CMD_CONNECT,1).
jack_control_open(Client) ->
    Cmd = tools:format("~s jack_control ~s", [code:priv_dir(studio) ++ "/studio.elf", Client]),
    open_port({spawn,Cmd},[{packet,1},binary,exit_status]).
loop(#{port := Port}=State) ->
    NextState =
        receive
            %% Port communication needs to be serialized.
            {Pid, {connect, Src, Dst}} ->
                Reply = rpc(
                          <<?JACK_CONTROL_CMD_CONNECT,Src/binary,0,Dst/binary,0>>,
                          Port),
                Pid ! {self(), obj_reply, Reply},
                State;
            %% All other messages implement the obj protocol.
            Msg ->
                obj:handle(Msg, State)
        end,
    loop(NextState).

rpc(Cmd, Port) ->
    Port ! {self(), {command, Cmd}},
    receive
        {Port,{exit_status,_}=E} -> exit(E);
        {Port,{data, Data}} -> Data
    end.


-module(jack_audio).
-export([start_link/2, handle/2]).

%% TODO
%% - put start_recorder/0 here
%% - link the recorder to the audio client

-define(CMD_SYNC,1).

start_link(Client,NI) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name(jack_audio),
                register(jack_audio, self()),  %% FIXME: is this necessary?
                Cmd = 
                    tools:format(
                      "~s jack_audio ~s ~p",
                      [jack_daemon:studio_elf(), Client, NI]),
                OpenPort =
                    [{spawn,Cmd},[{packet,4},binary,exit_status]],
                log:set_info_name({jack_audio, Client}),
                self() ! sync,
                handle(restart_port, #{ open_port => OpenPort })
        end,
        fun ?MODULE:handle/2})}.

handle(restart_port, State = #{open_port := Args}) ->
    case maps:find(port, State) of
        {ok, Port} ->
            Port ! {self(), {command, <<>>}},
            receive {Port, {exit_status, _}}=_E -> ok
            after 3000 -> exit({restart_port_timeout, Args})
            end;
        _ ->
            ok
    end,
    tools:info("start: ~p~n", [Args]),
    maps:put(port, apply(erlang,open_port,Args), State);
            

handle(exit, #{ port := Port } = State) ->
    Port ! {self(), {command, <<>>}},
    State;

handle({Port,{exit_status,_}=E}, #{ port := Port } = _State) ->
    exit(E);

handle({Port,{data,Msg}},
       #{ port := Port } = State) ->

    case maps:find(recorder, State) of
        {ok, Pid} -> Pid ! Msg;
        _ -> ok
    end,
    %% <<Type,Stamp,NbChan,NbFramesLog,Data/binary>> = Msg,
    %% log:info("audio: ~p~n",[{Type,Stamp,NbChan,NbFramesLog,size(Data)}]),
    handle(sync, State);

handle(sync, #{ port := Port } = State) ->
    Port ! {self(), {command, <<?CMD_SYNC>>}},
    State;

handle({record, _}, State = #{ recorder := Pid }) ->
    log:info("WARNING: already recording to ~p.~n",[Pid]), State;

handle({record, Pid}, State) ->
    _Ref = erlang:monitor(process, Pid),
    maps:put(recorder, Pid, State);


handle(Msg, State) ->
    obj:handle(Msg, State).




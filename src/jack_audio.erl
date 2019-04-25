-module(jack_audio).
-export([start_link/2, handle/2]).

%% TODO
%% - put start_recorder/0 here
%% - link the recorder to the audio client


start_link(Client,NI) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                Cmd = 
                    tools:format(
                      "~s jack_audio ~s ~p",
                      [studio_sup:studio_elf(), Client, NI]),
                OpenPort =
                    [{spawn,Cmd},[{packet,1},binary,exit_status]],
                log:set_info_name({jack_audio, Client}),
                handle(restart_port, #{ open_port => OpenPort })
        end,
        fun ?MODULE:handle/2})}.

%% See studio_sup:restart_port/1
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

%% Midi in. Translate to symbolic form.
handle({Port,{data,_}}, #{ port := Port } = State) ->
    State;

handle(Msg, State) ->
    obj:handle(Msg, State).




-module(port_midi).
-export([start_link/1, handle/2]).


%% Arbitrary raw midi sources.
%% Note: can't read from device, so /dev/midi? needs a port program.
init({socat, SocatTag, Node}) ->
    init(tools:format("socat - ~s:~s", [SocatTag, Node]),
              list_to_atom(Node));
init(Node) ->
    init({socat,"OPEN",Node}).
init(Cmd, Tag) ->
    Port=open_port({spawn,Cmd},[binary,exit_status]),
    #{port => Port, tag => Tag}.
handle({Port, {data, Data}},
            #{tag := Tag, port := Port}=State) ->
    tools:info("~p~n",[{Tag,midi:decode(Data)}]),
    State;
handle({data, Data}, #{port := Port}=State) ->
    Port ! {self(), {command, Data}},
    State.

start_link(Node) ->    
    {ok, serv:start(
           {handler,
            fun() -> init(Node) end,
            fun ?MODULE:handle/2})}.

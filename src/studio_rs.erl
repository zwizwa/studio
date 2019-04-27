-module(studio_rs).
-export([start_link/0, handle/2, call/2, call/1]).

%% Templated from exo_rs.erl

start_link(Host) ->
    {ok,
     serv:start(
       {handler,
        fun() -> handle(restart, #{ host => Host }) end,
        fun exo_rs:handle/2})}.

%% If it runs on localhost, register the process.
start_link() ->
    {ok, Pid} = start_link(home_bin),
    register(studio_rs, Pid),
    {ok, Pid}.



%% FIXME: This should rebuild the cache as well.
handle(restart, State = #{ host := Host }) ->
    case maps:find(port, State) of
        {ok, Port} -> port_close(Port);
        _ -> ok
    end,
    %% All port processes for exo go through a SSH layer.  This makes
    %% it simpler to decouple the Erlang side from the binary side.
    Opts = [use_stdio, binary, exit_status,{packet,4}],
    NewPort = exo:exo_open_port(Host, "studio_rs", Opts),
    Pid = self(),
    spawn_link(
      fun() -> 
              lists:foreach(
                fun({Ref,What}) ->
                        log:info("restoring: ~p~n", [{Ref,What}]),
                        %% Assert they are the same.
                        %% FIXME: The real solution is to ask for a particular slot.
                        {ok, Ref} = obj:call(Pid, {open, What})
                end,
                lists:sort(
                  [{Ref,What} || {{ref, Ref}, What} <- maps:to_list(State)]))
      end),
    maps:put(port, NewPort, State);

handle({Port, Msg}, #{port := Port} = _State) ->
    case Msg of
        {exit_status,_}=E ->
            exit(E);
        {data,_} ->
            %% All traffic from the port should be handle by
            %% etf_call/3, so this is an error
            exit({out_of_band,Msg})
    end;

%% Debug only
handle({_,dump}=Msg, State) ->
    obj:handle(Msg, State);

handle(close, #{ port := Port }) ->
    port_close(Port),
    exit(self(), closed);

handle(kill, _) ->
    exit(self(), kill);

%% Any other obj command is sent to the port.
handle({From, CmdTerm}, #{port := Port} = State) ->
    Reply = etf_call({port, Port}, CmdTerm, infinity),
    obj:reply(From, Reply),
    case {CmdTerm, Reply} of
        {_, {error, closed}} -> exit(Reply);
        {{open, What}, {ok, Ref}} -> maps:put({ref, Ref}, What, State);
        {{close, Ref}, _} -> maps:remove({ref, Ref}, State);
        _ -> State
    end.

etf_call({port,Port}, Term, Timeout) ->
    Ref = erlang:monitor(port, Port),
    Bin = term_to_binary(Term),
    true = port_command(Port, Bin),
    Val = receive
              {'DOWN',Ref,_,_,Reason} ->
                  {error, {'DOWN',Reason}};
              {Port,{data,Msg}} ->
                  try
                      case binary_to_term(Msg) of
                          {error, bad_command} ->
                              {error, {bad_command, Term}};
                          Other ->
                              Other
                      end
                  catch C:E -> {error, {binary_to_term, Msg, {C, E}}}
                  end;
              {Port, Other} ->
                  {error, Other}
          after
              Timeout ->
                  {error, timeout}
          end,
    erlang:demonitor(Ref, [flush]),
    Val.

call(Pid, Cmd) -> obj:call(Pid, Cmd).
call(Cmd) ->
    Pid = case whereis(exo_rs) of
              undefined -> exo:need(exo_rs);
              P -> P
          end,
    call(Pid, Cmd).



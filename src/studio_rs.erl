-module(studio_rs).
-export([start_link/1, handle/2, call/2, call/1]).


%% If it runs on localhost, register the process.
start_link(#{ spawn_port := _ }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                %% FIXME:  Reduce start pressure.
                timer:send_after(2000, restart),

                %% self() ! restart,
                maps:merge(
                  %% By default, start these rust subprocesses.
                  #{ {ref,0} => {jack_client,<<"studio_rs">>} },
                  Config)
        end,
        fun ?MODULE:handle/2})}.


handle(stop, State) ->
    case maps:find(port, State) of
        {ok, Port} ->
            log:info("stopping\n"),
            port_close(Port);
        error ->
            log:info("already stopped\n"),
            ok
    end,
    maps:remove(port, State);
handle(restart, State) ->
    self() ! start,
    handle(stop, State);

%% FIXME: This should rebuild the cache as well.
handle(start, State = #{ spawn_port := SpawnPort }) ->
    case maps:find(port, State) of
        {ok, _} ->
            log:info("already started\n"),
            State;
        error ->
            log:info("starting \n"),
            %% Framwork provides a way to start the port binary.
            NewPort =
                SpawnPort(
                  #{ cmd => "studio_rs.host.elf",
                     args => [],
                     dir => ["/i/exo/studio/rs"],
                     opts =>  [use_stdio, binary, exit_status,{packet,4}]
                   }),
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
            maps:put(port, NewPort, State)
    end;

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
    Pid = case whereis(?MODULE) of
              undefined -> exo:need(?MODULE);
              P -> P
          end,
    call(Pid, Cmd).

%% studio_rs:call({open,{jack_client,<<"studio_rs">>}}).
%% exo:connect(big, {jack_port, <<"studio_rs:midi_in">>}).


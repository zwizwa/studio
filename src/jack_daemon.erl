%% Jack Daemon wrapper.  Note that this insists on managing the
%% daemon.  I.e. it is "framework-y".

%% Some breadcrumbs:
%%
%% - In exo, this runs in the main supervisor.  Only jack_daemon is
%%   started, which then starts other jack_client instances.
%%
%% - The synth_tools/linux/hub.c jack_client sends jack events here,
%%   we call the notification function which in turn can send connect
%%   requests here.
%%
%% - The hub.c also handles MIDI processing in C, remainingin the jack
%%   data plane, but presenting a bridge to Erlang via tag_u32
%%   protocol and Erlang pterm.
%%

-module(jack_daemon).
-export([start_link/1, handle/2,
         start_client/2, client/1,
         start/1, stop/1]).


do_start(State) ->
    %% Precondition: not started
    error = maps:find(port, State),
    SH = code:priv_dir(studio) ++ "/start_jackd.sh",
    tools:info("jackd_open: ~s~n",[SH]),
    Opts = [{line,1024}, binary, use_stdio, exit_status],
    Port = open_port({spawn, SH}, Opts),
    %% FIXME: sync?
    timer:send_after(1000, need_clients),
    maps:put(port, Port, State).

start_link(Init = #{ }) ->
    {ok, serv:start(
           {handler,
            fun() -> 
                    log:set_info_name(?MODULE),
                    do_start(Init)
            end,
            fun ?MODULE:handle/2})}.

handle({Pid, start}, State) ->
    case maps:find(port, State) of
        {ok, Port} ->
            obj:reply(Pid, {ok, Port}),
            State;
        error ->
            State1 = do_start(State),
            obj:reply(Pid, {ok, maps:get(port, State1)}),
            State1
    end;
        

handle({Pid, stop}, State) ->
    State1 =
        case maps:find(port, State) of
            {ok, Port} ->
                port_close(Port),
                %% FIXME: This delay should be replaced by an RPC to the
                %% client, to ensure all external communication
                %% functionality (Jack connection, TCP service, ...) is
                %% down.
                timer:sleep(250),
                maps:remove(port, State);
            error ->
                %% Idempotent: already stopped.
                State
        end,
    obj:reply(Pid, ok),
    State1;

handle(need_clients, State) ->
    need_clients(State);

handle({Port, {data, Data}}, #{port := Port} = State) ->
    case Data of
        {eol, Line} ->
            log:info("~s~n", [Line]),
            handle({line, Line}, State);
        _ ->
            log:info("UNEXPECTED: Data=~p~n", [Data])
    end;
handle({Port, {exit_status, _}}=Msg, State = #{ port := Port }) ->
    log:info("WARNING: jack_daemon: ~p~n", [Msg]),
    %% Don't restart here
    maps:remove(port, State);



handle({line, _Line}, State) -> 
    %% tools:info("~s~n",[_Line]),
    State;
handle({client, Msg}, State) ->
    case Msg of
        {_,tc} -> dont_print;
        _ -> tools:info("jack client message: ~p~n",[Msg])
    end,
    State;

handle(Msg={_, {find, _}}, State) ->
    obj:handle(Msg, State);


%% ControlMsg from jack_client hub.c pterms are sent here because
%% Notify plug is stored here.  In exo, this links to
%% exo_midi:jack_notify/1 which looks up connectivity information in a
%% database and sends {connect,_,_} events here.
handle(_Msg = {jack_control, ControlMsg}, State = #{notify := Notify}) ->
    %% log:info("jack_daemon: ~p~n", [_Msg]),
    Notify(ControlMsg),
    State;
%% As sent by Notify explained above.
handle(Msg = {connect,_,_}, State) ->
    {Hub, State1} = hub_client(State),
    Hub ! Msg,
    State1;

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).

%% We only need the hub client.
hub_client(State) ->
    State1 = need_clients(State),
    Clients = maps:get(clients, State1),
    {maps:get(hub, Clients), State1}.


%% Clients are started on demand, to ensure it happens when daemon is
%% started.  Why are things split up?
%% - a2jmidid is an external application that we just reuse
%% - clock is kept seperate to ensure we design things to work as slave as well
%% - hub contains all routing code, and hosts some state machines (sequencer, stateful routing)
need_clients(State = #{ clients := _ }) ->
    State;
need_clients(State) ->
    tools:info("starting clients~n"),
    maps:put(
      clients,
      maps:from_list(
        [{Name,start_client(Name, State)}
         || Name <- [clock     %% synth_tools clock.c
                    ,hub       %% synth_tools hub.c (MIDI / Erlang hub)
                    ,a2jmidid  %% upstream alsa to jack midi bridge
                    ]]),
      State).

jack_client_proc(State, Name) ->
    Pid = exo:need({jack_client, Name}),
    %% The jack_client wrapper no longer starts the port process
    %% automatically, so we do that here.
    jack_client:start(Pid),
    {ok, Pid}.

start_client(Name, State) ->
    {ok, Pid} = jack_client_proc(State, atom_to_binary(Name)),
    Pid.

client(Name) ->
    maps:get(Name, obj:get(jack_daemon, clients)).


stop(Pid) -> obj:call(Pid, stop).
start(Pid) -> obj:call(Pid, start).

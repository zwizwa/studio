%% Jack Daemon wrapper.  Note that this insists on managing the
%% daemon.  I.e. it is "framework-y".

%% Some breadcrumbs:
%%
%% - In exo, this runs in the main supervisor.  Only jack_daemon is
%%   stared, which then starts other functionality.
%%
%% - Exo also starts midi_raw, which is independent of all jack code.
%%   Its purpose is to be a hub for midi devices that are not
%%   connected to jack.
%%
%% - While jack daemon is starting up, the daemon's stdout is parsed
%%   and for each 'added' line, handle_connect/4 is called, which...
%%
%% - ... lazy starts jack_control, jack_midi and jack_audio clients.
%%
%% - jack_control handles port/alias events
%%
%% - jack_midi is a C port and Erlang wrapper that does some "data
%%   plane" midi operations (e.g. clock generation, sequencer, sysex)
%%   inside the C application, and furthermore bridges the Jack MIDI
%%   world and the Erlang message world.  Some thought has been put in
%%   here, so have a look at jack_midi.c
%%
%% - jack_audio is currently a dummy memcpy audio sink



%% Notes
%%
%% - This evolved in a very ad-hoc way.  I currently do not have the
%%   time to redesign it.  I guess it is ok, just that startup is a
%%   little messy.
%%
%% - It is probably possible to remove the stdout parsing, but at this
%%   time it is still used to ensure the jack clients are only started
%%   once the daemon is up.  Once control deamon is up, events are
%%   handled that way.


-module(jack_daemon).
-export([start_link/1, handle/2,
         start_client/2, client/1]).


%% Wrap the daemon and listen on its stdout as a simple way to get
%% MIDI port connect notifications.

%% Once midi port aliases are known, connect them to a specified port
%% number on the jack client.

start_link(Init = #{ hubs := _,
                     spawn_port := _}) ->
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
    SH = code:priv_dir(studio) ++ "/start_jackd.sh",
    tools:info("jackd_open: ~s~n",[SH]),
    Opts = [{line,1024}, binary, use_stdio, exit_status],
    Port = open_port({spawn, SH}, Opts),
    %% FIXME: sync?
    timer:send_after(1000, need_clients),
    maps:put(port, Port, State);

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
    log:info("WARNING: ~p~n", [Msg]),
    timer:send_after(2000, start),
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

handle(Msg={_,dump}, State) ->
    obj:handle(Msg, State).


control_client(State) ->
    State1 = need_clients(State),
    Clients = maps:get(clients, State1),
    {maps:get(control, Clients), State1}.

%% Clients are started on demand, to ensure it happens when daemon is
%% started.
need_clients(State = #{ clients := _ }) ->
    State;
need_clients(State) ->
    tools:info("starting clients~n"),
    maps:put(
      clients,
      maps:from_list(
        [{Name,start_client(Name, State)}
         || Name <- [control   %% RPC
                    ,clock     %% synth_tools clock.c
                    ,hub       %% synth_tools hub.c (MIDI / Erlang hub)
                    ,synth     %% synth_tools synth.c
                    ,a2jmidid  %% upstream alsa to jack midi bridge
                    ]]),
      State).

jack_client_proc(#{ spawn_port := SpawnPort }, Name) ->
    %% Let exo start the client.
    %% FIXME: Maybe abstract the spawn mechanism?
    %% jack_client:proc(#{name => Name, spawn_port => SpawnPort}).
    Pid = exo:need({jack_client, Name}),

    %% The jack_client wrapper no longer starts the port process
    %% automatically, so we do that here.
    jack_client:start(Pid),

    {ok, Pid}.
    

start_client(Name, State=#{ hubs := Hubs, notify := Notify, spawn_port := SpawnPort }) ->
    {ok, Pid} = 
        case Name of

            %% FIXME: Should these be started here?

            %% Main MIDI clock source (synth_tools)
            clock -> jack_client_proc(State, <<"clock">>);

            %% Centralized midi hub from synth_tools.
            hub   -> jack_client_proc(State, <<"hub">>);

            %% Example MIDI synth (synth_tools)
            synth -> jack_client_proc(State, <<"synth">>);

            %% Upstream alsa to jack midi bridge
            a2jmidid -> jack_a2jmidid:start_link(#{});

            %% RPC jack interface
            control ->
                jack_control:start_link(
                  #{client => "studio_control",
                    notify => Notify
                   })
        end,
    Pid.

client(Name) ->
    maps:get(Name, obj:get(jack_daemon, clients)).

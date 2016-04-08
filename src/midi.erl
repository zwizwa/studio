-module(midi).
-export([alsa_seq_in/1, alsa_seq_in/2, alsa_seq_handle/2,
         jack_control/1, jack_control_loop/1,
         jack_midi/4, jack_midi_handle/2, %% jack midi client
         jackd_init/0, jackd_handle/2, %% jackd wrapper
         jackd_port_start_link/0, jackd_port_handle/2, %% jackd erlang port wrapper
         decode/2,decode/1,encode/1,
         port_start_link/1, port_handle/2,
         hub_start_link/0]).
%% Original idea was to route messages over broadcast, but that works
%% horribly over wifi.  It seems best to make a translation to erlang
%% on the host the device is plugged into, and then distribute it
%% there.

%% int(X) -> list_to_integer(binary_to_list(X)).

-define(IF(C,A,B),if C -> A; true -> B end).


%% Accumulator task for incremental encoders.
%% maps_inc(Tag,Inc,Map) ->
%%     Prev = maps:get(Tag,Map,0),
%%     Val = Prev+Inc,
%%     {Val,maps:put(Tag,Val,Map)}.



%% Decode a chunk.  Assume messages are complete.
%% https://www.midi.org/specifications/item/table-1-summary-of-midi-message


lh(L,H) -> L + (H bsl 7).

-define(OFF, 8).
-define(ON,  9).
-define(PKP,10).  %% Polyphonic Key Pressure
-define(CC, 11).  %% Continuous Controller
-define(CP, 12).  %% Channel Pressure
-define(PB, 14).  %% Pitch Bend

-define(TC,    16#F8). %% Timing Clock (24 / quarter note)
-define(START, 16#FA).
-define(CONT,  16#FB).
-define(STOP,  16#FC).


decode(<<?OFF:4, C:4, K:8, V:8, R/binary>>, S)  -> [{off,  C, K, V}  | decode(R, S)];
decode(<<?ON :4, C:4, K:8, V:8, R/binary>>, S)  -> [{on,   C, K, V}  | decode(R, S)];
decode(<<?PKP:4, C:4, K:8, V:8, R/binary>>, S)  -> [{pkp,  C, K, V}  | decode(R, S)];
decode(<<?CC :4, C:4, K:8, V:8, R/binary>>, S)  -> [{cc,   C, K, V}  | decode(R, S)];
decode(<<?CP :4, C:4, V:8,      R/binary>>, S)  -> [{cp,   C,    V}  | decode(R, S)];
decode(<<?PB :4, C:4, L:8, H:8, R/binary>>, S)  -> [{pb,   C,lh(L,H)}| decode(R, S)];
decode(<<?TC    :8,             R/binary>>, S)  -> [tc               | decode(R, S)];
decode(<<?START :8,             R/binary>>, S)  -> [start            | decode(R, S)];
decode(<<?CONT  :8,             R/binary>>, S)  -> [cont             | decode(R, S)];
decode(<<?STOP  :8,             R/binary>>, S)  -> [stop             | decode(R, S)];

decode(<<_:8,R/binary>>,S) -> decode(R,S);
decode(<<>>,S) -> S.

decode(Bin) -> decode(Bin,[]).


encode({off,  C, K, V})  -> <<?OFF:4, C:4, K:8, V:8>>;
encode({on,   C, K, V})  -> <<?ON :4, C:4, K:8, V:8>>;
encode({pkp,  C, K, V})  -> <<?PKP:4, C:4, K:8, V:8>>;
encode({cc,   C, K, V})  -> <<?CC :4, C:4, K:8, V:8>>;
encode({cp,   C,    V})  -> <<?CP :4, C:4, V:8>>;
%%encode({pb,   C,lh(L,H)  -> <<?PB :4, C:4, L:8, H:8>>;
encode(tc             )  -> <<?TC    :8>>;
encode(start          )  -> <<?START :8>>;
encode(cont           )  -> <<?CONT  :8>>;
encode(stop           )  -> <<?STOP  :8>>;
encode(_) -> <<>>.
    
    

%% Alsa sequencer client, midi in.
%% Only a single listener is necessary: distinguish based on source addr.
%% E.g. use aconnect in udev?
alsa_open(Client) ->
    open_port({spawn,"priv/studio alsa_seq_in " ++ Client},
              [{packet,1},binary,exit_status]).
alsa_seq_in(Client, Sink) ->
    serv:start({handler,
                fun() -> {alsa_open(Client), Sink} end,
                fun midi:alsa_seq_handle/2}).
alsa_seq_in(Client) ->
    alsa_seq_in(Client,
                fun(Msg) -> tools:info("~p~n",[Msg]) end).
alsa_seq_handle({Port,{data,Data}},{Port,Sink}=State) ->
    alsa_seq_route(Data, Sink), State.
alsa_seq_route(<<Type,_Flags,_Tag,_Queue,
                 _TimeStamp:64/little,
                 SC,SP,_DC,_DP,
                 Data/binary>>, Sink) ->
    alsa_seq_ev(Type,{seq,{SC,SP}},Data,Sink).
alsa_seq_ev(6, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{on, C,K,V}});
alsa_seq_ev(7, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{off,C,K,V}});
alsa_seq_ev(10,S,<<C,_,_,_,K:32/little,V:32/signed-little,_/binary>>,Sink) -> Sink({S,{cc, C,K,V}});
alsa_seq_ev(Type,S,Data,Sink) -> Sink({S,{Type,Data}}).
    


%% JACK midi client. Preferred as it has better timing properties.
%% Supports midi in/out, clock generation, and jack client connection.
-define(JACK_MIDI_CMD_MIDI,0).
-define(JACK_MIDI_CMD_CONNECT,1).
jack_midi_open(Client,NI,NO,ClockMask) ->
    Cmd = tools:format("priv/studio jack_midi ~s ~p ~p ~p", [Client, NI, NO, ClockMask]),
    open_port({spawn,Cmd},[{packet,1},binary,exit_status]).
jack_midi(Client,NI,NO,ClockMask) ->
    serv:start({handler,
                fun() -> jack_midi_open(Client,NI,NO,ClockMask) end,
                fun midi:jack_midi_handle/2}).
jack_midi_handle(exit, Port) ->
    Port ! {self(), {command, <<>>}},
    Port;
jack_midi_handle({Port,{exit_status,_}=E},Port) ->
    exit(E);
%% Midi in. Translate to symbolic form.
jack_midi_handle({Port,{data,<<MidiPort,Data/binary>>}}, Port) ->
    lists:foreach(
      fun(Msg) -> serv:hub_send(midi_hub, {{jack,MidiPort},Msg}) end,
      decode(Data)),
    Port;

%% Midi out
jack_midi_handle({midi,Mask,Data}, Port) ->
    Bin = ?IF(is_binary(Data), Data, encode(Data)),
    Port ! {self(), {command, <<Mask:32/little, Bin/binary>>}},
    Port.



%% Jack control client
-define(JACK_CONTROL_CMD_CONNECT,1).
jack_control_open(Client) ->
    Cmd = tools:format("~s/studio jack_control ~s",
                       [code:priv_dir(studio),Client]),
    open_port({spawn,Cmd},[{packet,1},binary,exit_status]).
jack_control(Client) ->
    serv:start({body, 
                fun() -> midi:jack_control_loop(
                           #{port => jack_control_open(Client)})
                end}).
jack_control_loop(#{port := Port}=State) ->
    NextState =
        receive
            %% Port communication needs to be serialized.
            {Pid, {connect, Src, Dst}} ->
                Reply = jack_control_rpc(
                          <<?JACK_CONTROL_CMD_CONNECT,Src/binary,0,Dst/binary,0>>,
                          Port),
                Pid ! {self(), obj_reply, Reply},
                State;
            %% All other messages implement the obj protocol.
            Msg ->
                obj:handle(Msg, State)
        end,
    jack_control_loop(NextState).
jack_control_rpc(Cmd, Port) ->
    Port ! {self(), {command, Cmd}},
    receive
        {Port,{exit_status,_}=E} -> exit(E);
        {Port,{data, Data}} -> Data
    end.




%% Jack I/O name mapper
%% Easy enough to observe stdout of the server process for port add/delete.
%% Once midi port aliases are known, connect them to a specified port number on the jack client.
jackd_init() ->
    #{}.
jackd_handle({line, <<"scan: ", Rest/binary>>}, State) ->
    {match,[_|[Action,_,Dir,Addr,Name]]} =
        re:run(Rest,
               <<"(\\S+) port (\\S+) (\\S+)\\-(hw\\-\\d+\\-\\d+\\-\\d+)\\-(\\S+)\n*">>,
               [{capture,all,binary}]),
    PortAlias = <<Dir/binary,$-,Addr/binary,$-,Name/binary>>,
    Key = {Dir, Name},
    %%tools:info("~s ~p => ~p~n",[Action, Key, PortAlias]),
    case Action of
        <<"added">> ->
            S1=maps:put(Key,PortAlias,State),
            jackd_connect(PortAlias, Dir, Name, S1);
        <<"deleted">> ->
            maps:remove(Key, State);
        _ ->
            State
    end;
jackd_handle({line, _Line}, State) -> 
    tools:info("~s~n",[_Line]),
    State;
jackd_handle({client, Msg}, State) ->
    case Msg of
        {_,tc} -> dont_print;
        _ -> tools:info("jack client message: ~p~n",[Msg])
    end,
    State;
jackd_handle(Msg, State) ->
    obj:handle(Msg, State).
jackd_connect(PortAlias, Dir, Name, State) ->
    {C,S} = jackd_need_client(State),
    N = integer_to_binary(db:port_id(Name)),
    %% tools:info("~p~n",[[PortAlias,Dir,Name,N]]),
    Connect = fun(Src,Dst) ->
                      spawn(
                        fun() ->
                                %% Port creation seems to happen after
                                %% it is logged to the console.  Can't
                                %% sync, so wait.
                                timer:sleep(500),
                                obj:call(C,{connect,Src,Dst})
                        end)
              end,
                      
    case Dir of
        <<"in">>  -> Connect(PortAlias,<<"studio:midi_in_",N/binary>>);
        <<"out">> -> Connect(<<"studio:midi_out_",N/binary>>,PortAlias)
    end,
    S.
jackd_need_client(#{control := Client}=State) ->
    {Client, State};
jackd_need_client(State) ->
    tools:info("starting client~n"),
    ClockMask = db:midiclock_mask(),
    jackd_need_client(
      maps:merge(
        State,
        #{control => jack_control("studio_control"),
          midi    => jack_midi("studio",16,16,ClockMask)})).



%% Start jackd as a port, and connect it to jack_update/2.
%% Alternatively, use: socat EXEC:$JACKD TCP-CONNECT:localhost:13000
%% in combination with linemon.erl
jackd_open() ->
    open_port({spawn, "jackd.local"},
              [{line,1024}, binary, use_stdio, exit_status]).
jackd_port_start_link() ->
    {ok, serv:start(
           {handler,
            fun() -> #{port => jackd_open()} end,
            fun midi:jackd_port_handle/2})}.
jackd_port_handle({Port, {data, {eol, Line}}}, #{port := Port} = State) ->
    jackd_handle({line, Line}, State);
jackd_port_handle({Port, {exit_status, _}=Msg}, #{port := Port}) ->
    exit(Msg);
jackd_port_handle(Msg, State) ->
    %% tools:info("jackd_port_handle: ~p ~p~n",[Msg,State]),
    jackd_handle(Msg, State).






%% Arbitrary raw midi sources.
%% Note: can't read from device, so /dev/midi? needs a port program.
port_init({socat, SocatTag, Node}) ->
    port_init(tools:format("socat - ~s:~s", [SocatTag, Node]),
              list_to_atom(Node));
port_init(Node) ->
    port_init({socat,"OPEN",Node}).
port_init(Cmd, Tag) ->
    Port=open_port({spawn,Cmd},[binary,exit_status]),
    #{port => Port, tag => Tag}.
port_handle({Port, {data, Data}},
            #{tag := Tag, port := Port}=State) ->
    tools:info("~p~n",[{Tag,decode(Data)}]),
    State;
port_handle({data, Data}, #{port := Port}=State) ->
    Port ! {self(), {command, Data}},
    State.
port_start_link(Node) ->    
    {ok, serv:start({handler,
                     fun() -> port_init(Node) end,
                     fun midi:port_handle/2})}.



        

not_tc({_,Msg}) -> Msg =/= tc.

%% Midi hub.  serv:hub object filters at the source
hub_start_link() ->
    Pid = serv:hub_start(), 
    register(midi_hub, Pid),
    serv:hub_add(Pid, fun not_tc/1, serv:info_start()),
    {ok, Pid}.

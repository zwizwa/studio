-module(midi).
-export([alsa_seq_in/1, alsa_seq_in/2, alsa_seq_handle/2,
         jack/4, jack/3, jack_handle/2, %% jack midi client
         jack_ports/0, %% obsolete
         jackd_init/0, jackd_handle/2, %% jackd wrapper
         jackd_port_start/0, jackd_port_handle/2, %% jackd erlang port wrapper
         decode/2,decode/1,encode/1,
         port_start/1, port_handle/2]).
%% Original idea was to route messages over broadcast, but that works
%% horribly over wifi.  It seems best to make a translation to erlang
%% on the host the device is plugged into, and then distribute it
%% there.

%% int(X) -> list_to_integer(binary_to_list(X)).


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
    
    

%% Alsa sequencer, midi in.
%% Only a single listener is necessary: distinguish based on source addr.
%% E.g. use aconnect in udev?
alsa_seq_in(Client, Sink) ->
    serv:start({handler,
                fun() ->
                        {open_port({spawn,"priv/studio alsa_seq_in " ++ Client},
                                   [{packet,1},binary,exit_status]),
                         Sink}
                end,
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

%% Decode events into our tuple representation.
alsa_seq_ev(6, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{on, C,K,V}});
alsa_seq_ev(7, S,<<C,K,V,_/binary>>,Sink) -> Sink({S,{off,C,K,V}});
alsa_seq_ev(10,S,<<C,_,_,_,K:32/little,V:32/signed-little,_/binary>>,Sink) -> Sink({S,{cc, C,K,V}});
alsa_seq_ev(Type,S,Data,Sink) -> Sink({S,{Type,Data}}).
    

%% JACK midi. Preferred as it has better timing properties.
jack(Client,NI,NO,Sink) ->
    Cmd = tools:format("priv/studio jack_midi ~s ~p ~p",
                       [Client, NI, NO]),
    serv:start(
      {handler,
       fun() ->{open_port(
                  {spawn,Cmd},
                  [{packet,1},binary,exit_status]),
                Sink}
       end,
       fun midi:jack_handle/2}).
jack(Client,NI,NO) ->
    jack(Client,NI,NO,
         fun(Msg) -> tools:info("~p~n",[Msg]) end).
jack_handle(exit, {Port,_}=State) ->
    Port ! {self(), {command, <<>>}},
    State;
jack_handle({Port,{exit_status,_}=E},{Port,_}) ->
    exit(E);
jack_handle({send,Mask,Data},
            {Port,_}=State) ->
    Bin = if
              is_binary(Data) -> Data;
              true -> encode(Data)
          end,
    Port ! {self(), {command,
                     <<Mask:32/little,
                       Bin/binary>>}},
    State;
jack_handle({Port,{data,<<MidiPort,Data/binary>>}},
                 {Port,Sink}=State) ->
    lists:foreach(
      fun(Msg) -> Sink({{jack,MidiPort},Msg}) end,
      decode(Data)),
    State.
jack_ports() ->
    %% FIXME: there doesn't seem to be a better way to get to real
    %% names, so resort to whack-a-mole.  requires jackd to be started
    %% with stdout>/tmp/jackd.log
    {ok, Ports} = tools:script_lines("cat /tmp/jackd.log |grep 'added port' |awk '{ print $5 }'",1000),
    {Ins, Outs} = lists:partition(fun(L) -> hd(L) == <<"in">> end, [re:split(P,"-") || P<- Ports]),
    Strip = fun([_,_,_,_,_|Ws]) -> string:join([binary_to_list(W) || W<-Ws],"_") end,
    Map = fun(L,Tag) ->
                  maps:from_list(
                    [{list_to_atom(Strip(Name)),
                      list_to_atom(tools:format("system:midi_~s_~p",[Tag,N+1]))}
                     ||{N,Name} <- tools:enumerate(L)])
          end,
    {Map(Ins, "capture"),
     Map(Outs,"playback")}.


%% Jack I/O map.


%% Arbitrary midi sources.
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
port_start(Node) ->    
    serv:start({handler,
                fun() -> port_init(Node) end,
                fun midi:port_handle/2}).




%% Jack state update, fed from jackd stdout.
jackd_init() ->
    #{}.
jackd_handle({line, <<"scan: ", Rest/binary>>}, State) ->
    {match,[_|Event]} =
        re:run(Rest,
               <<"(\\S+) port (\\S+) (\\S+)\\-hw\\-\\d+\\-\\d+\\-\\d+\\-(\\S+)\n*">>,
               [{capture,all,binary}]),
    %%tools:info("jack_midi: ~p~n", [Event]),
    [Action,_,Dir,Name] = Event,
    N = maps:get({next_port, Dir}, State, 1),
    case Action of
        <<"added">> ->
            NameFmt = case Dir of
                          <<"in">>  -> "midi_capture_~p";
                          <<"out">> -> "midi_playback_~p"
                      end,
            PortName = tools:format(NameFmt,[N]),
            S=maps:merge(State,
                         #{{next_port, Dir} => N+1,
                           {Name,Dir} => PortName}),
            tools:info("jack: ~s ~p => ~p~n",[Action, {Name,Dir},PortName]),
            S;
        <<"deleted">> ->
            S=maps:remove({Name,Dir}, State),
            tools:info("jack: ~s ~p~n",[Action, {Name,Dir}]),
            S;
        _ ->
            State
    end;
%% Not clear what we should sync to exactly, but the point is to start
%% our client once the daemon is running.
jackd_handle({line,<<"Acquire audio card ", _Card/binary>>}, State) ->
    Self = self(),
    Sink = fun(Msg) -> Self ! {client, Msg} end,
    maps:put(client, jack("studio",16,16,Sink), State);

jackd_handle({line,_Msg}, State) ->
    %% tools:info("jack: ~p~n", [{_Msg,State}]),
    State;

%% We can annotate client's midi messages.
jackd_handle({client, _Msg}, State) ->
    %%tools:info("jack client message: ~p~n",[_Msg]),
    State;

jackd_handle(Msg, State) ->
    %% tools:info("jackd_handle: ~p ~p~n",[Msg, State]),
    obj:handle(Msg, State).


%% Start jackd as a port, and connect it to jack_update/2.
%% Alternatively, use: socat EXEC:$JACKD TCP-CONNECT:localhost:13000
%% in combination with linemon.erl
jackd_port_start() ->
    serv:start(
      {handler,
       fun() -> #{
            port =>
                open_port({spawn, "jackd.local"},
                          [{line,1024}, binary, use_stdio, exit_status])}
       end,
       fun midi:jackd_port_handle/2}).
jackd_port_handle({Port, {data, {eol, Line}}}, #{port := Port} = State) ->
    jackd_handle({line, Line}, State);
jackd_port_handle({Port, {exit_status, _}=Msg}, #{port := Port}) ->
    exit(Msg);
jackd_port_handle(Msg, State) ->
    %% tools:info("jackd_port_handle: ~p ~p~n",[Msg,State]),
    jackd_handle(Msg, State).

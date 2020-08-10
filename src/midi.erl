%% Generic midi code.


-module(midi).
-export([%%alsa_seq_in/1, alsa_seq_in/2, alsa_seq_handle/2,
         %%jack_control/1, jack_control_loop/1,
         %%jack_midi/4, jack_midi_handle/2, %% jack midi client
         %%jackd_init/0, jackd_handle/2, %% jackd wrapper
         %% jackd_port_start_link/0, jackd_port_handle/2, %% jackd erlang port wrapper
         decode/2,decode/1,encode/1,
         %% port_start_link/1, port_handle/2,
         start_link/0, handle/2,
         trigger_start/2, trigger_cc/1,
         not_tc/1,
         note_freq/1,
         sysex_encode/1, sysex_decode/1, sysex_decode_framed/1
         %%,db/0,sql/1,port_id/1,midiclock_mask/0
        ]).
%% Original idea was to route messages over broadcast, but that works
%% horribly over wifi.  It seems best to make a translation to erlang
%% on the host the device is plugged into, and then distribute it
%% there.

%% int(X) -> list_to_integer(binary_to_list(X)).

-define(IF(C,A,B), (case (C) of true -> (A); false -> (B) end)).


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

encode(Unknown) -> throw({midi_encode,Unknown}).
    
    










        

not_tc({midi,_,_,tc}) -> false;
not_tc(_) -> true.
    

%% Midi hub.  serv:hub object filters at the source

%% NOTE: direct epid connections are much more convenient, so this is
%% no longer used.



start_link() ->
    %% Extend serv broacaster with handling of pids.
    BC = serv:start(
           {handler,
            fun serv:bc_init/0,
            fun ?MODULE:handle/2}),
    Info = serv:info_start(),
    BC ! {subscribe, {Info, fun ?MODULE:not_tc/1}},
    {ok, BC}.

handle(Msg, State) ->
    serv:bc_handle(Msg, State).


%% Event trigger, e.g. for midi learn.
trigger_start(Pred, Cont) ->
    serv:start(
      {body,
       fun() -> 
               midi_hub ! {subscribe, {self(), Pred}},
               receive Msg -> Cont(Msg) end
       end}).
                   
trigger_cc({_,{cc,_,_,_}}) -> true;
trigger_cc(_) -> false.

%% Encode binary as MSB-prefixed sysex.
sysex_encode(Bin) -> 
    lists:map(
      fun({Start,Size}) ->
              Chunk    = binary:part(Bin, Start, Size),
              ChunkPad = binary:part(<<Chunk/binary, 0,0,0,0,0,0,0,0>>, 0, 7),
              [A,B,C,D,E,F,G] = [V bsr 7 || <<V>> <= ChunkPad],
              LSBs = [V band 127 || <<V>> <= Chunk],
              [<<0:1,G:1,F:1,E:1,D:1,C:1,B:1,A:1>>|LSBs]
      end,
      tools:nchunks(0, size(Bin), 7)).
    

sysex_decode(Bin) ->
    lists:map(
      fun({Start,Size}) ->
              <<MSB,Chunk/binary>> = binary:part(Bin, Start, Size),
              LSBs = binary_to_list(Chunk),
              {MSBs,_} = lists:split(
                           size(Chunk), 
                           lists:reverse([V || <<V:1>> <= <<MSB>>])),
              lists:map(
                fun({H,L}) -> (H bsl 7) bor (L band 127) end,
                lists:zip(MSBs,LSBs))
      end,
      tools:nchunks(0, size(Bin), 8)).

%% FIXME: Untested.
sysex_decode_framed(<<16#F0,Manufacturer,Rest/binary>>) ->
    Size = size(Rest),
    16#F7 = binary:at(Rest, Size-1),
    {Manufacturer, sysex_decode(binary:part(Rest, 0, Size-1))}.


note_freq(Note) ->
    440.0 * math:pow(2,(Note - 69) / 12.0).
    

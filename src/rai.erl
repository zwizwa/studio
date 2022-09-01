-module(rai).
-export([%% run_udp/1, handle_udp/2,
         proc/1, handle_proc/2,
         %% to_pd/2,
         emacs_set_rai_buffer_pid/2,
         get/2,
         epid_app/2,
         export_makefile/2,
         export_makefile/0
        ]).

%% Code moved from exo repo to studio repo.
%% See exo repo dbce63b2607070abb919346e22c6c1912b7466fb for last revision.
%% This code has Pd protocol and old epid code removed.
%% RAI uses uc_tools TAG_U32 protocol.

%% The ELF binaries are assumed to be in rai_uc_tools directory in the
%% CWD.  There is currently no way to change that.

-define(RAI_OUTPUT_TAG,out).
-define(TAG_U32,16#FFF5).

-define(ID_PARAM,0).
-define(ID_SET,0).
-define(ID_GET,1).


%% Start a processor or synth.
proc(#{ type := _Type, %% jack, pulse, ...
        name := Name,  %% basename
        spawn_port := _SpawnPort }=Config) ->
    {ok,
     serv:start(
       {handler,
        fun() ->
                log:set_info_name({rai,Name}),
                lists:foreach(
                  fun(Pid) -> Pid ! {subscribe, {self(), fun midi:not_tc/1}} end,
                  exo:pids(midi_hub)),
                Self = self(),
                Self ! start,
                Self ! emacs_connect,
                spawn_link(fun() -> update_cache_task(Self) end),
                Config
        end,
        fun ?MODULE:handle_proc/2})}.

update_cache_task(Pid) ->
    try
        {Dir, _Meta} = tag_u32:dir(Pid, [param]),
        Cache = maps:from_list(
                  [{K,V} || {V,{K,param}} <- tools:enumerate(Dir)]),
        Pid ! {set_param_cache, Cache}
    catch
        C:E ->
            log:info("~p~n",[{C,E}])
    end.

handle_proc(start, #{ type := Type, name := Name, spawn_port := SpawnPort } = State) ->
    case maps:find(port, State) of
        {ok, _Port} ->
            State;
        error ->
            Cmd = tools:format("~s.~s.host.elf",[Name,Type]),
            log:info("Cmd = ~p~n", [Cmd]),
            maps:put(
              port,
              SpawnPort(
                #{ dir  => "rai_uc_tools",
                   cmd  => Cmd,
                   args => [],
                   opts => [use_stdio, binary, exit_status, {packet,4}] }),
              State)
    end;

handle_proc(stop, State) ->
    log:info("stop\n"),
    case maps:find(port, State) of
        {ok, Port} ->
            port_close(Port);
        error ->
            ok
    end,
    maps:remove(port, State);

handle_proc(restart, State) ->
    lists:foldl(
      fun(Cmd, S) -> handle_proc(Cmd, S) end,
      State,
      [stop, start]);

handle_proc({Port,{exit_status,_}=E}, State = #{port := Port}) ->
    %% Don't crash the process, just issue a warning.
    log:info("WARNING: ~p~n",[E]),
    maps:remove(port, State);

handle_proc(emacs_connect, State = #{ name := Name }) ->
    register_emacs(Name, self()),
    State;

handle_proc(Msg={_,dump},State) ->
    obj:handle(Msg, State);


%% Param cache is available.  Use it to convert the AList to a
%% sequence of messages that are sent asynchronously.  
handle_proc({set,AList}=_Msg, State = #{param := Param, port := Port}) -> 
    log:info("~p~n",[_Msg]),
    %% FIXME: Just assumed, not actually resolved.
    ParamDir = 0, 
    SetCmd   = 0,
    lists:foreach(
      fun({Key,Val}) ->
              ID = maps:get(Key, Param, Key),
              case is_integer(ID) of
                  true ->
                      Path = [?ID_PARAM,ID,?ID_SET],
                      IOL = tag_u32:tag_u32(?TAG_U32,Path,<<Val:32/float>>),
                      true = port_command(Port, IOL),
                      ok;
                  false ->
                      log:info("bad param: ~p~n", [{Key,Val}])
              end
      end,
      AList),
    State;

%% Param cache is not available.  Buffer the messages until we know
%% where to send them.
handle_proc({set,_AList}=Msg, State) ->
    Buf = maps:get(buf, State, []),
    maps:put(buf, [Msg|Buf], State);

%% Cache needs to be built up by a sequence of RPCs against this
%% object, so we sequence it in a separate object and install it after
%% startup, flushing the buffer of messages that might have arrived
%% meanwhile, in order.
handle_proc(_Msg={set_param_cache, Cache}, State) ->
    log:info("~p~n",[_Msg]),
    Buf = maps:get(buf, State, []),
    State1 = lists:foldl(fun handle_proc/2, State,
                         lists:reverse(Buf)),
    maps:merge(State1, #{ param => Cache, buf => []});

%% Epid names are just parameter names or number.
handle_proc({epid_send,Sink,Val}, State) when is_atom(Sink) ->
    handle_proc({set,[{Sink,Val}]}, State);

%% Delegate to mixins at tail end.
handle_proc(Msg, State) -> 
    Mixins = [fun epid:mixin/3, fun tag_u32:mixin/3],
    {Handled, State1} = serv:delegate(Mixins, Msg, State),
    case Handled of
        true ->
            State1;
        false ->
            log:info("unknown: ~p~n",[Msg]),
            State1
    end.






%% to_pd(N,Val)      when is_number(N) and is_number(Val) -> io_lib:format("p~p ~p;\n", [N, Val]);
%% to_pd(Var,Val)    when is_atom(Var) and is_number(Val) -> io_lib:format("~p ~p;\n", [Var, Val]);
%% to_pd({cc,N},Val) when is_number(N) and is_number(Val) -> io_lib:format("cc~p ~p;\n", [N, Val]);

%% to_pd(cc,{N,Val}) when is_number(N) and is_number(Val) -> to_pd({cc,N},Val); %% Grouping transposition

%% to_pd(K,V) ->
%%     log:info("ignoring invalid {Var,Val} pair: ~999p~n", [{K,V}]),
%%     [].



%% FIXME: Handle a message that connects to emacs and sets the buffer
%% local variable associated to the filename.  See rai.el



emacs_set_rai_buffer_pid(Name,Pid) ->
    emacs:distel_send_lisp(
      ['save-excursion',
       ['set-buffer', Name],
       ['setq', 'rai-pid', Pid]]).
register_emacs(Name, Pid) ->
    try
        %% Assume emacs uses plain filename.
        [Base|_] = re:split(Name, "\\."),
        BufName = tools:format("~s.rkt", [Base]),
        log:info("connecting to emacs buffer ~p~n", [BufName]),
        emacs_set_rai_buffer_pid(BufName, Pid)
    catch C:E ->
            log:info("WARNING: register_emacs: ~p~n", [{C,E}])
    end.



%% %% UDP Handler.  Don't use this from emacs.  Use distel instead to
%% %% send messages directly to pid.

%% %% FIXME: This is a singleton.  Support multiple modules.
%% run_udp(PortNo) ->
%%     {ok,
%%      serv:start(
%%        {handler,
%%         fun() ->
%%                 {ok, Sock} = gen_udp:open(PortNo, [binary, {active,true}]),
%%                 #{ sock => Sock}
%%         end,
%%         fun ?MODULE:handle_udp/2})}.
%% handle_udp({udp,Sock,_Host,_Port,Data}, State = #{ sock := Sock }) ->
%%     %% FIXME: Currently this only takes Pd messages for a single instance.
%%     tools:re_case(
%%       Data,
%%       [{"\\s*(.*?)\\s+(.*?)\\s*;\n",
%%         fun([_Var,Val]) -> 
%%                 {_FVal,[]} = string:to_float(Val),
%%                 %% log:info("~p~n", [{_Var,_FVal}]),
                
%%                 %% Looking up every time is inefficient.  Why not just
%%                 %% send it out to all nodes?
%%                 lists:foreach(
%%                   fun(Pid) -> Pid ! {pd, Data} end,
%%                   exo:pids({rai, 
%%                             %% 'doodle.pulse'
%%                             'controlspace.pulse'
%%                             %% 'synth.pulse'
%%                            })),

%%                 %% {proc, 'exo@10.1.3.23'} ! {pd, Data},
%%                 State
%%         end},
%%        {"",
%%         fun(_) -> exit({bad_data, data}) end}]);

%% handle_udp(Msg, State) ->
%%     obj:handle(Msg, State).


epid_app({rai, Name}, _InputEpids) ->
    %% FIXME: Instances!
    %% FIXME: only jack for now
    Pid = exo:need({rai,jack,Name}, 'exo@10.1.3.2'),
    Epid = {epid, Pid, out},
    #{ tmp => [], out => Epid }.



get(Pid, ParamName) ->
    case tag_u32:call(Pid, [param,ParamName,get]) of
        {[0],<<Val:32/float>>} ->
            {ok, Val};
        _ ->
            error
    end.
        

%% Note: uc_tools is always assumed to be available as a subdirectory
%% (or link) and writable, as build products always go next to the
%% source files.  That was an intentional design choice to keep
%% makefiles simpler.  If separate build directories are necessary
%% then link or copy uc_tools file into build directory.

export_makefile(Redo, Targets) ->
    Rebase = #{
      [<<"rai_uc_tools">>,<<"rai">>] => [<<".">>],
      [<<"rai">>] => [<<"..">>]
     },

    redo:pull(Redo, Targets),
    IOL = redo:export_makefile(Redo, Targets, Rebase),
    %% FIXME: find a better place to put this

    %% This is "gen.mk" - There is an edited wrapper Makefile
    Makefile = "/i/exo/deps/rai/rai_uc_tools/rules.mk", 
    log:info("writing ~s~n", [Makefile]),
    file:write_file(
      Makefile,
      ["# DO NOT EDIT.\n",
       "# Generated from upstream erl_tools/src/redo.erl build. Ask Tom.\n",
       IOL]).

%% Bound to exo dev setup
f(Str) -> redo:from_filename(Str).
export_makefile() ->
    export_makefile(
      redo,
      [f("rai/rai_uc_tools/doodle.jack.host.elf")]).

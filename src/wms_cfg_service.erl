%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Configuration service for WMS system.
%%% @end
%%% Created : 02. May 2019 11:48
%%%-------------------------------------------------------------------
-module(wms_cfg_service).
-author("Attila Makra").
-behaviour(gen_server).

-include_lib("wms_logger/include/wms_logger.hrl").

%% API
-export([start_link/0,
         load_config/3,
         reload_config/1,
         get/3,
         set/3,
         clear/0,
         set_protected/2]).

-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2]).

%% =============================================================================
%% Private types
%% =============================================================================

-record(state, {
  mode = undefined :: atom(),
  files = [] :: [string()],
  variables = #{} :: map(),
  manual_variables = #{} :: map(),
  protected_deps = #{} :: map(),
  backup :: term()
}).
-type state() :: #state{}.
-type key_value() :: {term(), term() | [key_value()]}.
-type app_mode() :: {Mode :: atom(), [key_value()]}.
-type file_app_entry() :: {App :: atom(), [app_mode()]}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec load_config(atom(), [string()], reset | additive) ->
  ok |{error, term()}.
load_config(Mode, FileList, LoadMode) ->
  gen_server:call(?MODULE, {load_config, Mode, FileList, LoadMode}).

-spec reload_config(atom()) ->
  ok | {error, term()}.
reload_config(Mode) ->
  gen_server:call(?MODULE, {reload_config, Mode}).

-spec get(atom(), term() | [term()], term()) ->
  term().
get(Application, Keys, Default) ->
  gen_server:call(?MODULE, {get, Application, Keys, Default}).

-spec set(atom(), term() | [term()], term()) ->
  ok.
set(Application, Keys, Value) ->
  gen_server:call(?MODULE, {set, Application, Keys, Value}).

-spec clear() ->
  ok.
clear() ->
  gen_server:call(?MODULE, clear).

-spec set_protected(atom(), boolean()) ->
  ok.
set_protected(Application, Logical) ->
  gen_server:call(?MODULE, {set_protected, Application, Logical}).

%% =============================================================================
%% gen_server behaviour
%% =============================================================================
-spec init(Args :: term()) ->
  {ok, State :: state()}.
init(_) ->
  {ok, #state{}}.

-spec handle_info(Info :: any(), State :: state()) ->
  {noreply, State :: state()}.
handle_info(_, State) ->
  {noreply, State}.

-spec handle_call(Info :: any(), From :: {pid(), term()}, State :: state())
                 ->
                   {reply, term(), State :: state()}.
handle_call({reload_config, Mode}, _From, #state{files = FileList} = State) ->
  NewState = (backup(State))#state{
    variables = #{},
    mode      = Mode,
    files     = []
  },
  do_load_config(Mode, FileList, NewState);

handle_call({load_config, Mode, FileList, reset}, _From, State) ->
  NewState = (backup(State))#state{
    variables = #{},
    mode      = Mode,
    files     = []
  },
  do_load_config(Mode, FileList, NewState);

handle_call({load_config, Mode, FileList, additive}, _From,
            #state{mode = undefined} = State) ->
  % first call load call was overload, before mode was set
  NewState = (backup(State))#state{
    mode = Mode
  },
  do_load_config(Mode, FileList, NewState);

handle_call({load_config, Mode, FileList, additive}, _From,
            #state{mode  = OldMode,
                   files = OldFileList} = State) when Mode =/= OldMode ->
  % mode changed, most reload all files
  NewState = (backup(State))#state{
    mode      = Mode,
    files     = [],
    variables = #{}
  },
  do_load_config(Mode, merge_files(OldFileList, FileList), NewState);

handle_call({load_config, Mode, FileList, additive}, _From, State) ->
  do_load_config(Mode, FileList, backup(State));

handle_call({get, Application, Keys, Default}, _From,
            #state{variables = Vars} = State) ->
  Path = to_path(Keys, Application),
  {reply, maps:get(Path, Vars, Default), State};

handle_call({set, Application, Keys, Value}, _From,
            #state{variables        = Vars,
                   manual_variables = MVars} = State) ->
  Path = to_path(Keys, Application),
  {reply, ok, State#state{
    variables        = Vars#{Path => Value},
    manual_variables = MVars#{Path => Value}}};

handle_call(clear, _From, #state{mode = Mode}) ->
  ?debug("Clear all configuration variables"),
  {reply, ok, #state{mode = Mode}};

handle_call({set_protected, Application, Logical}, _From,
            #state{protected_deps = Protected} = State) ->
  NewProtected =
    case Logical of
      true ->
        ?debug("~s application config sets protected", [Application]),
        Protected#{Application => true};
      false ->
        ?debug("~s application config unset protected", [Application]),
        maps:remove(Application, Protected)
    end,
  {reply, ok, State#state{protected_deps = NewProtected}}.

-spec to_path(term() | [term()], atom()) ->
  [term()].
to_path(Keys, Application) ->
  case is_list(Keys) of
    true ->
      [Application | Keys];
    false ->
      [Application, Keys]
  end.

merge_files(OldFileList, FileList) ->
  OldFileList ++ lists:subtract(FileList, OldFileList).

-spec handle_cast(Request :: any(), State :: state())
                 ->
                   {noreply, State :: state()}.
handle_cast(_, State) ->
  {noreply, State}.

%% =============================================================================
%% Private functions
%% =============================================================================

-spec do_load_config(atom(), [string()], state()) ->
  {reply, term(), state()}.
do_load_config(Mode, FileList, #state{variables        = OldVariables,
                                      files            = OldFileList,
                                      backup           = BackupState,
                                      manual_variables = MVars,
                                      protected_deps   = ProtectedDeps} = State) ->
  {Reply, NewState} =
    case load_files(Mode, FileList, ProtectedDeps) of
      {ok, Variables} ->
        State1 = State#state{
          files     = merge_files(OldFileList, FileList),
          variables = maps:merge(OldVariables, Variables)
        },
        {ok, apply_manual_vars(State1, MVars)};
      Other ->
        {Other, BackupState}
    end,

  {reply, Reply, NewState}.

-spec load_files(atom(), [string()], map()) ->
  {ok, map()} | {error, term()}.
load_files(Mode, FileList, ProtectedDeps) ->
  load_files(Mode, FileList, ProtectedDeps, #{}).

-spec load_files(atom(), [string()], map(), map()) ->
  {ok, map()} | {error, term()}.
load_files(_Mode, [], _, Vars) ->
  {ok, Vars};
load_files(Mode, [File | Rest], ProtectedDeps, Vars) ->
  case file:consult(File) of
    {ok, Content} ->
      NewVars = load_entry_content(Mode, Content, ProtectedDeps, Vars),
      load_files(Mode, Rest, ProtectedDeps, NewVars);
    Else ->
      Else
  end.

-spec load_entry_content(atom(), [file_app_entry()], map(), map()) ->
  map().
load_entry_content(_Mode, [], _, Vars) ->
  Vars;
load_entry_content(Mode, [{Application, AppModes} | RestEntries],
                   ProtectedDeps, Vars) ->
  case maps:is_key(Application, ProtectedDeps) of
    false ->
      % read app settings for default mode
      AppDefaultEntries = proplists:get_value(default, AppModes, []),
      % read app settings for given modes
      AppModeEntries = proplists:get_value(Mode, AppModes, []),

      EntryMapDefault = wms_common:proplist_to_map(AppDefaultEntries, Application),
      EntryMapMode = wms_common:proplist_to_map(AppModeEntries, Application),
      EntryMap = maps:merge(EntryMapDefault, EntryMapMode),

      ?debug("~s application was loaded", [Application]),

      load_entry_content(Mode, RestEntries, ProtectedDeps,
                         maps:merge(Vars, replace(EntryMap)));
    true ->
      ?debug("~s application was not loaded, "
             "because configuration protected", [Application]),
      load_entry_content(Mode, RestEntries, ProtectedDeps, Vars)
  end.

-spec replace(map()) ->
  map().
replace(EntryMap) ->
  HostName = list_to_binary(wms_common:get_hostname()),
  maps:fold(
    fun(K, V, CurrMap) ->
      V1 = replace_hostname(V, HostName),
      V2 = replace_env_var(V1),
      CurrMap#{K => V2}
    end, #{}, EntryMap).

-spec replace_hostname(term() | [term()], binary()) ->
  term().
replace_hostname(V, HostName) when is_list(V) ->
  lists:reverse(
    lists:foldl(
      fun(E, Acc) ->
        [replace_hostname(E, HostName) | Acc]
      end, [], V));
replace_hostname(V, HostName) when is_atom(V) ->
  AtomToBinary = atom_to_binary(V, latin1),
  case binary:match(AtomToBinary, <<"__hostname__">>) of
    nomatch ->
      V;
    _ ->
      binary_to_atom(binary:replace(AtomToBinary,
                                    <<"__hostname__">>,
                                    HostName), latin1)
  end;
replace_hostname(V, _) ->
  V.

-spec replace_env_var(term() | [term()]) ->
  term().
replace_env_var(V) when is_list(V) ->
  lists:reverse(
    lists:foldl(
      fun(E, Acc) ->
        [replace_env_var(E) | Acc]
      end, [], V));
replace_env_var(V) when is_atom(V) ->
  AtomToBinary = atom_to_binary(V, latin1),
  case get_env_var_pos(AtomToBinary) of
    nomatch ->
      V;
    {Start, Length} ->
      binary_to_atom(replace_env_var(AtomToBinary, Start, Length), latin1)
  end;
replace_env_var(V) ->
  V.

-spec replace_env_var(binary(), integer(), integer()) ->
  binary().
replace_env_var(Reference, Start, Length) ->
  EnvVarName = binary_to_list(binary:part(Reference, Start, Length)),
  case os:getenv(EnvVarName) of
    false ->
      ?error("~s environment variable was not set", [EnvVarName]),
      Reference;
    Value ->
      Before = binary:part(Reference, 0, Start - 2),
      After = binary:part(Reference,
                          Start + Length + 1,
                          byte_size(Reference) - (Start + Length + 1)),
      <<Before/binary, (list_to_binary(Value))/binary, After/binary>>
  end.

get_env_var_pos(V) ->
  Position =
    case binary:match(V, <<"${">>) of
      nomatch ->
        nomatch;
      {Start, _} ->
        {Start, binary:match(V, <<"}">>, [{scope, {Start, byte_size(V) - Start}}])}
    end,

  case Position of
    nomatch ->
      nomatch;
    {_, nomatch} ->
      nomatch;
    {St, {End, _}} ->
      {St + 2, End - St - 2}
  end.

-spec apply_manual_vars(state(), map()) ->
  state().
apply_manual_vars(#state{variables = Vars} = State, MVars) ->
  State#state{variables = maps:merge(Vars, MVars)}.

backup(State) ->
  BackedUp = State#state{backup = undefined},
  State#state{backup = BackedUp}.

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

%% API
-export([start_link/0,
         load_config/3,
         reload_config/1,
         get/3]).

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

-spec get(atom(), term() | [term()], key_value()) ->
  key_value().
get(Application, Keys, Default) ->
  gen_server:call(?MODULE, {get, Application, Keys, Default}).


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
handle_call({reload_config, Mode}, From, #state{files = FileList} = State) ->
  handle_call({load_config, Mode, FileList, reset},
              From, State#state{variables = #{},
                                mode      = Mode,
                                backup    = State});

handle_call({load_config, Mode, FileList, reset}, From, State) ->
  handle_call({load_config, Mode, FileList, additive},
              From, State#state{variables = #{},
                                files     = [],
                                mode      = Mode,
                                backup    = State});

handle_call({load_config, Mode, FileList, additive}, From,
            #state{mode  = OldMode,
                   files = OldFileList} = State) when Mode =/= OldMode ->
  % mode changed, most reload all files
  NewState = State#state{
    mode      = Mode,
    files     = [],
    variables = #{},
    backup    = State
  },
  handle_call({load_config, Mode, merge_files(OldFileList, FileList), additive},
              From, NewState);

handle_call({load_config, Mode, FileList, additive}, _From,
            #state{variables = OldVariables,
                   files     = OldFileList,
                   backup    = BackupState}) ->
  {Reply, NewState} =
    case load_files(Mode, FileList) of
      {ok, Variables} ->
        {ok, #state{
          files     = merge_files(OldFileList, FileList),
          variables = maps:merge(OldVariables, Variables)
        }};
      Other ->
        {Other, BackupState}
    end,

  {reply, Reply, NewState};

handle_call({get, Application, Keys, Default}, _From,
            #state{variables = Vars} = State) ->
  Path = case is_list(Keys) of
           true ->
             [Application | Keys];
           false ->
             [Application, Keys]
         end,
  {reply, maps:get(Path, Vars, Default), State}.

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
-spec load_files(atom(), [string()]) ->
  {ok, map()} | {error, term()}.
load_files(Mode, FileList) ->
  load_files(Mode, FileList, #{}).

-spec load_files(atom(), [string()], map()) ->
  {ok, map()} | {error, term()}.
load_files(_Mode, [], Vars) ->
  {ok, Vars};
load_files(Mode, [File | Rest], Vars) ->
  case file:consult(File) of
    {ok, Content} ->
      NewVars = load_entry_content(Mode, Content, Vars),
      load_files(Mode, Rest, NewVars);
    Else ->
      Else
  end.

%%-spec load_file_content(atom(), [list()], map()) ->
%%  map().
%%load_file_content(_Mode, [], Vars) ->
%%  Vars;
%%load_file_content(Mode, [Entry | Rest], Vars) ->
%%  NewVars = load_entry_content(Mode, Entry, Vars),
%%  load_entry_content(Mode, Rest, NewVars).

-spec load_entry_content(atom(), [file_app_entry()], map()) ->
  map().
load_entry_content(_Mode, [], Vars) ->
  Vars;
load_entry_content(Mode, [{Application, AppModes} | RestEntries], Vars) ->
  % read app settings for given modes
  AppModeEntries = proplists:get_value(Mode, AppModes,
                                       proplists:get_value(default, AppModes, [])),
  EntryMap = wms_common:proplist_to_map(AppModeEntries, Application),
  load_entry_content(Mode, RestEntries, maps:merge(Vars, EntryMap)).
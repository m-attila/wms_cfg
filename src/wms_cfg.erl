%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% API functions for wms_cfg applications.
%%% @end
%%% Created : 02. May 2019 12:42
%%%-------------------------------------------------------------------
-module(wms_cfg).
-author("Attila Makra").

-include_lib("wms_logger/include/wms_logger.hrl").

%% API
-export([load_config/2,
         reload_config/1,
         get/3,
         overload_config/2,
         get_mode/0,
         set/3,
         load_app_config/1,
         clear/0,
         get_app_directory/1,
         set_protected/2,
         start_apps/2]).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% load_config/2
%% ###### Purpose
%% Clear current config settings and load configuration files for given mode.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec load_config(atom(), [string()]) ->
  ok |{error, term()}.
load_config(Mode, FileList) ->
  ?info("Load configuration files : ~p, ~p", [Mode, FileList]),
  wms_cfg_service:load_config(Mode, FileList, reset).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% load_app_config/1
%% ###### Purpose
%% Load applications config to upgrade current configuration setting
%% and set protected state of apps-
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec load_app_config([atom()]) ->
  ok | no_return().
load_app_config(Applications) ->
  do_load_app_config(Applications),
  [set_protected(App, true) || App <- Applications],
  ?info("Config are loaded for ~0p applications", [Applications]),
  ok.

-spec do_load_app_config([atom()]) ->
  ok | no_return().
do_load_app_config([]) ->
  ok;
do_load_app_config([Dep | RestDeps]) ->
  FileName = atom_to_list(Dep) ++ ".config",
  Path = filename:join(wms_cfg:get_app_directory(Dep), FileName),
  ok = case filelib:is_file(Path) of
         true ->
           overload_config(wms_cfg:get_mode(), [Path]);
         false ->
           ?warning("No config file was found: ~p", [Path]),
           ok
       end,
  do_load_app_config(RestDeps).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_app_directory/1
%% ###### Purpose
%% Returns private directory for given application.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_app_directory(atom()) ->
  string().
get_app_directory(Dep) ->
  code:priv_dir(Dep).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% overload_config/2
%% ###### Purpose
%% Upgrade config variables for given mode with configuration files.
%% If mode will be changed, all loaded configuration files will be
%% reload before upgrade.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec overload_config(atom(), [string()]) ->
  ok |{error, term()}.
overload_config(Mode, FileList) ->
  ?info("Overload configuration files : ~p, ~p", [Mode, FileList]),
  wms_cfg_service:load_config(Mode, FileList, additive).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% reload_config/1
%% ###### Purpose
%% Reload all loaded configuration files for given mode.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec reload_config(atom()) ->
  ok | {error, term()}.
reload_config(Mode) ->
  ?info("Reload configuration : ~p", [Mode]),
  wms_cfg_service:reload_config(Mode).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get/3
%% ###### Purpose
%% Returns config variable or when missing, default value for given app.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get(atom(), term() | [term()], term()) ->
  term().
get(Application, Keys, Default) ->
  wms_cfg_service:get(Application, Keys, Default).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% set/3
%% ###### Purpose
%% Sets config variable for given app.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec set(atom(), term() | [term()], term()) ->
  ok.
set(Application, Keys, Value) ->
  wms_cfg_service:set(Application, Keys, Value).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% set_protected/2
%% ###### Purpose
%% Sets protection of config variables for given application.
%% If application is protected, configuration variables will not be
%% modified when load new configuration file.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec set_protected(atom(), boolean()) ->
  ok.
set_protected(Application, Logical) ->
  wms_cfg_service:set_protected(Application, Logical).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_mode/0
%% ###### Purpose
%% Retuns system mode
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_mode() ->
  atom().
get_mode() ->
  case os:getenv("wms_mode") of
    false ->
      ?error("CFG-0001", "wms_mode environment variable was not set ~s.", [wms_cfg]),
      throw("wms_mode environment variable was not set.");
    Value ->
      list_to_atom(Value)
  end.

-spec clear() ->
  ok.
clear() ->
  wms_cfg_service:clear().


-spec start_apps(atom(), [atom()]) ->
  ok | {error, term()}.
start_apps(Application, Deps) ->
  wms_cfg:load_app_config(Deps ++ [Application]),

  Ret = [application:ensure_all_started(App) || App <- Deps],

  {Succ, Fail} = lists:partition(
    fun({ok, _}) ->
      true;
       (_) ->
         false
    end,
    Ret),

  case Fail of
    [] ->
      StartedApps = lists:foldr(
        fun({ok, A}, Acc) ->
          A ++ Acc
        end, [], Succ),
      ?info("Applications started : ~0p", [StartedApps]),
      ok;
    Errors ->
      Errors = lists:foldr(
        fun({error, A}, Acc) ->
          A ++ Acc
        end, [], Errors),
      ?error("CFG-0002", "Failed to start applications: ~0p", [Errors]),
      {error, Errors}
  end.

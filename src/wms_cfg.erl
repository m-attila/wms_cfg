%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% API functions for wms_cfg applications.
%%% @end
%%% Created : 02. May 2019 12:42
%%%-------------------------------------------------------------------
-module(wms_cfg).
-author("Attila Makra").

%% API
-export([load_config/2,
         reload_config/1,
         get/3,
         overload_config/2,
         get_mode/0,
         set/3]).

-spec load_config(atom(), [string()]) ->
  ok |{error, term()}.
load_config(Mode, FileList) ->
  wms_cfg_service:load_config(Mode, FileList, reset).

-spec overload_config(atom(), [string()]) ->
  ok |{error, term()}.
overload_config(Mode, FileList) ->
  wms_cfg_service:load_config(Mode, FileList, additive).

-spec reload_config(atom()) ->
  ok | {error, term()}.
reload_config(Mode) ->
  wms_cfg_service:reload_config(Mode).

-spec get(atom(), term() | [term()], term()) ->
  term().
get(Application, Keys, Default) ->
  wms_cfg_service:get(Application, Keys, Default).

-spec set(atom(), term() | [term()], term()) ->
  ok.
set(Application, Keys, Value) ->
  wms_cfg_service:set(Application, Keys, Value).

-spec get_mode() ->
  atom().
get_mode() ->
  case os:getenv("wms_mode") of
    false ->
      throw("wms_mode environment variable was not set.");
    Value ->
      list_to_atom(Value)
  end.


%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Test suites for wms_cfg.
%%% @end
%%% Created : 29. Apr 2019 08:12
%%%-------------------------------------------------------------------
-module(wms_cfg_SUITE).
-author("Attila Makra").

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("wms_cfg.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
  [{key, value}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  [{test_dir, code:lib_dir(?APP_NAME) ++ "/test"} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
  ?MODULE:GroupName({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(GroupName, Config) ->
  ?MODULE:GroupName({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
  [
    {start_stop_group,
     [{repeat_until_any_fail, 1}],
     [
       start_stop_test
     ]
    },
    {config_group,
     [{repeat_until_any_fail, 1}],
     [
       get_mode_test,
       config_test,
       multi_config_test
     ]
    }
  ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
  [
    {group, start_stop_group},
    {group, config_group}
  ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------

%% =============================================================================
%% Start-stop group
%% =============================================================================

start_stop_group({prelude, Config}) ->
  Config;
start_stop_group({postlude, _}) ->
  ok.

%%--------------------------------------------------------------------
%% Start, stop application.
%%
%%--------------------------------------------------------------------

%% test case information
start_stop_test({info, _Config}) ->
  [""];
start_stop_test(suite) ->
  ok;
%% init test case
start_stop_test({prelude, Config}) ->
  Config;
%% destroy test case
start_stop_test({postlude, _Config}) ->
  ok;
%% test case implementation
start_stop_test(_Config) ->
  {ok, _} = application:ensure_all_started(?APP_NAME),
  ok = application:stop(?APP_NAME).

%% =============================================================================
%% Config group
%% =============================================================================
config_group({prelude, Config}) ->
  {ok, _} = application:ensure_all_started(?APP_NAME),
  Config;
config_group({postlude, _}) ->
  ok = application:stop(?APP_NAME).

%%--------------------------------------------------------------------
%% Get mode test
%%
%%--------------------------------------------------------------------

%% test case information
get_mode_test({info, _Config}) ->
  [""];
get_mode_test(suite) ->
  ok;
%% init test case
get_mode_test({prelude, Config}) ->
  [{save_mode, os:getenv("wms_mode")} | Config];
%% destroy test case
get_mode_test({postlude, Config}) ->
  case ?config(save_mode, Config) of
    false ->
      os:unsetenv("wms_mode");
    Value ->
      os:putenv("wms_mode", Value)
  end,
  ok;
%% test case implementation
get_mode_test(_Config) ->
  os:unsetenv("wms_mode"),
  ?assertException(throw, _, wms_cfg:get_mode()),
  os:putenv("wms_mode", "test"),
  ?assertEqual(test, wms_cfg:get_mode()).

%%--------------------------------------------------------------------
%% Configuration services test
%%
%%--------------------------------------------------------------------

%% test case information
config_test({info, _Config}) ->
  [""];
config_test(suite) ->
  ok;
%% init test case
config_test({prelude, Config}) ->
  Config;
%% destroy test case
config_test({postlude, _Config}) ->
  ok;
%% test case implementation
config_test(Config) ->
  % load test data
  BaseFile = filename:join(?config(test_dir, Config), "base.config"),
  ok = wms_cfg:load_config(test, [BaseFile]),

  % default entry found
  Expected0 = 1000,
  ?assertEqual(Expected0, wms_cfg:get(app1, node_connection_timeout, not_found)),

  % replaced by hostname
  ExpectedHostname = list_to_atom("wms_node@" ++ wms_common:get_hostname()),
  ?assertEqual([ExpectedHostname], wms_cfg:get(app1, host, not_found)),

  % entry found
  Expected1 = [test1, test2],
  ?assertEqual(Expected1, wms_cfg:get(app1, nodes, not_found)),

  % entry not found
  ?assertEqual(not_found, wms_cfg:get(app1, no_key, not_found)),

  % env_var1 contains wms_mode
  ?assertEqual('test', wms_cfg:get(app1, env_var1, not_found)),
  ?assertEqual('atest', wms_cfg:get(app1, env_var2, not_found)),
  ?assertEqual('testa', wms_cfg:get(app1, env_var3, not_found)),
  ?assertEqual('${notfound}', wms_cfg:get(app1, env_var4, not_found)),

  % reload config in production mode
  wms_cfg:reload_config(prod),
  Expected2 = [prod1, prod2],
  ?assertEqual(Expected2, wms_cfg:get(app1, nodes, not_found)),

  % add extra configuration data, mode changed back to 'test'
  ExtraFile = filename:join(?config(test_dir, Config), "extra_entries.config"),
  ok = wms_cfg:overload_config(test, [ExtraFile]),

  % default timeout changed
  Expected10 = 1500,
  ?assertEqual(Expected10, wms_cfg:get(app1, node_connection_timeout, not_found)),


  % node names remain
  Expected1 = [test1, test2],
  ?assertEqual(Expected1, wms_cfg:get(app1, nodes, not_found)),

  % timeout added
  ?assertEqual(15000, wms_cfg:get(app1, timeout, not_found)),

  % app2 has config variable
  ?assertEqual(test, wms_cfg:get(app2, mode, not_found)),

  % add extra2 config data with same mode

  ExtraFile2 = filename:join(?config(test_dir, Config), "extra_entries2.config"),
  ok = wms_cfg:overload_config(test, [ExtraFile2]),

  % default timeout changed
  Expected30 = 2000,
  ?assertEqual(Expected30, wms_cfg:get(app1, node_connection_timeout, not_found)),


  % node names changed
  Expected3 = [newtest1, newtest2],
  ?assertEqual(Expected3, wms_cfg:get(app1, nodes, not_found)),

  % timeout remains
  ?assertEqual(15000, wms_cfg:get(app1, timeout, not_found)),

  % load only extra2 file
  ok = wms_cfg:load_config(test, [ExtraFile2]),

  % node names remains
  ?assertEqual(Expected3, wms_cfg:get(app1, nodes, not_found)),

  % timeout not found
  ?assertEqual(not_found, wms_cfg:get(app1, timeout, not_found)),

  % long key
  ?assertEqual(connstr1, wms_cfg:get(app1, [database, db1], not_found)),

  % bad filename on load
  {error, enoent} = wms_cfg:load_config(test, ["bad_file.txt"]),

  % config values remains valid after load error

  % node names remains
  ?assertEqual(Expected3, wms_cfg:get(app1, nodes, not_found)),

  % timeout not found
  ?assertEqual(not_found, wms_cfg:get(app1, timeout, not_found)),

  % long key
  ?assertEqual(connstr1, wms_cfg:get(app1, [database, db1], not_found)),

  % bad filename on overload
  {error, enoent} = wms_cfg:overload_config(test, ["bad_file.txt"]),
  % config values remains valid after load error

  % node names remains
  ?assertEqual(Expected3, wms_cfg:get(app1, nodes, not_found)),

  % timeout not found
  ?assertEqual(not_found, wms_cfg:get(app1, timeout, not_found)),

  % long key
  ?assertEqual(connstr1, wms_cfg:get(app1, [database, db1], not_found)),

  % set variable to application
  ?assertEqual(not_found, wms_cfg:get(app1, [k1, k2], not_found)),
  ok = wms_cfg:set(app1, [k1, k2], value),
  ?assertEqual(value, wms_cfg:get(app1, [k1, k2], not_found)),

  ?assertEqual(not_found, wms_cfg:get(app1, x1, not_found)),
  ok = wms_cfg:set(app1, x1, value),
  ?assertEqual(value, wms_cfg:get(app1, x1, not_found)),

  ok.

%%--------------------------------------------------------------------
%% Load config for all dependencies
%%
%%--------------------------------------------------------------------

%% test case information
multi_config_test({info, _Config}) ->
  [""];
multi_config_test(suite) ->
  ok;
%% init test case
multi_config_test({prelude, Config}) ->
  ok = meck:new(wms_cfg, [passthrough]),
  SourceDir = filename:join(code:lib_dir(?APP_NAME), "test"),
  DestDir = code:priv_dir(?APP_NAME),
  {ok, _} = file:copy(filename:join(SourceDir, "app1.config"),
                      filename:join(DestDir, "app1.config")),
  {ok, _} = file:copy(filename:join(SourceDir, "app2.config"),
                      filename:join(DestDir, "app2.config")),

  meck:expect(wms_cfg, get_app_directory, fun(_) ->
    DestDir end),

  [{dest_dir, DestDir} | Config];
%% destroy test case
multi_config_test({postlude, Config}) ->
  ok = meck:unload(wms_cfg),
  DestDir = ?config(dest_dir, Config),
  ok = file:delete(filename:join(DestDir, "app1.config")),
  ok = file:delete(filename:join(DestDir, "app2.config"));
%% test case implementation
multi_config_test(_Config) ->
  wms_cfg:clear(),
  ok = wms_cfg:load_app_config([app1]),
  ?assertEqual(node1, wms_cfg:get(app1, node, not_found)),
  ?assertEqual(2, wms_cfg:get(app1, timeout, not_found)),
  ?assertEqual(not_found, wms_cfg:get(app1, repeat, not_found)),
  ?assertEqual(not_found, wms_cfg:get(app2, size, not_found)),

  wms_cfg:clear(),
  ok = wms_cfg:load_app_config([app1, app2]),
  ?assertEqual(node2, wms_cfg:get(app1, node, not_found)),
  ?assertEqual(5, wms_cfg:get(app1, timeout, not_found)),
  ?assertEqual(10, wms_cfg:get(app1, repeat, not_found)),
  ?assertEqual(1000, wms_cfg:get(app2, size, not_found)),

  % config already protected
  ok = wms_cfg:load_app_config([app1]),
  ?assertEqual(node2, wms_cfg:get(app1, node, not_found)),
  ?assertEqual(5, wms_cfg:get(app1, timeout, not_found)),
  ?assertEqual(10, wms_cfg:get(app1, repeat, not_found)),
  ?assertEqual(1000, wms_cfg:get(app2, size, not_found)).






%% -------------------------------------------------------------------
%%
%% Kesto: High Availability Monitoring Software.
%%
%% Copyright (c) 2012 Conversion Co., Ltd.  All Rights Reserved.
%% Copyright (c) 2013 Kesto Project  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @author Takahiro Shidara <takahoge@gmail.com>
%% @doc 定義取得モジュール

-module(kesto_scheduler_config).

-export([
		 check_interval/0,
		 check_cycle/0,
		 check_offset/0
		]).

%% @spec check_interval() -> integer() | error
%% @doc スケジュール定義取得間隔
check_interval() ->
	get_kesto_scheduler_env(check_interval).

%% @spec check_cycle() -> integer() | error
%% @doc スケジュール定義チェック周期
check_cycle() ->
	get_kesto_scheduler_env(check_cycle).

%% @spec check_offset() -> integer() | error
%% @doc スケジュール定義取得間隔オフセット
check_offset() ->
	get_kesto_scheduler_env(check_offset).

%% @private
get_kesto_scheduler_env(Key) ->
	app_helper:get_env(kesto_scheduler, Key).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

kesto_scheduler_config_test_() ->
	{ setup,
	  fun setup/0,
	  fun cleanup/1,
	  [
	   fun check_interval_test_case/0,
	   fun check_cycle_test_case/0,
	   fun check_offset_test_case/0
	  ]
	}.

check_interval_test_case() ->
	application:set_env(kesto_scheduler, check_interval, 60000),
	?assertEqual(60000, check_interval()).

check_cycle_test_case() ->
	application:set_env(kesto_scheduler, check_cycle, 1000),
	?assertEqual(1000, check_cycle()).

check_offset_test_case() ->
	application:set_env(kesto_scheduler, check_offset, 30000),
	?assertEqual(30000, check_offset()).

setup() ->   
	application:load(kesto_scheduler).

cleanup(_Pid) ->
	application:stop(kesto_scheduler).

-endif.

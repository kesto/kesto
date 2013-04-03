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

-module(kesto_monitor_config).

-export([
		 fping_path/0,
		 fping6_path/0
		]).

%% @spec fping_path() -> string() | error
%% @doc fping(IPv4)のパスを取得する。
fping_path() ->
	get_kesto_monitor_env(fping_path).

%% @spec fping6_path() -> string() | error
%% @doc fping(IPv6)のパスを取得する。
fping6_path() ->
	get_kesto_monitor_env(fping6_path).

%% @private
get_kesto_monitor_env(Key) ->
   app_helper:get_env(kesto_monitor, Key).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

riak_core_config_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
	   fun fping_path_test_case/0,
	   fun fping6_path_test_case/0
      ]
    }.

fping_path_test_case() ->
	application:set_env(kesto_monitor, fping_path, "sudo /usr/local/sbin/fping"),
	?assertEqual("sudo /usr/local/sbin/fping", fping_path()).

fping6_path_test_case() ->
	application:set_env(kesto_monitor, fping6_path, "sudo /usr/local/sbin/fping6"),
	?assertEqual("sudo /usr/local/sbin/fping6", fping6_path()).

setup() ->   
    application:load(kesto_monitor).

cleanup(_Pid) ->
    application:stop(kesto_monitor).
    
-endif.

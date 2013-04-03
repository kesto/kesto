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

-module(kesto_reciever_config).

-export([
		 syslog_port/0
		]).

%% @spec syslog_port() -> {integer()} | error
%% @doc Syslog受信ポート番号を取得する。
syslog_port() ->
	get_kesto_reciever_env(syslog_port).

%% @private
get_kesto_reciever_env(Key) ->
	app_helper:get_env(kesto_reciever, Key).

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
	   fun syslog_port_test_case/0
	  ]
	}.

syslog_port_test_case() ->
	application:set_env(kesto_reciever, syslog_port, 10514),
	?assertEqual(10514, syslog_port()).

setup() ->   
	application:load(kesto_reciever).

cleanup(_Pid) ->
	application:stop(kesto_reciever).

-endif.

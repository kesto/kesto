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

-module(kesto_core_config).

-export([
		 put_option/0,
		 smtp_option/0,
		 mail_from/0,
		 ssh_client_connect_timeout/0,
		 expiration_date/0
		]).

%% @spec put_option() -> [] | error
%% @doc riak_objectのputオプションを取得する。
put_option() ->
	get_kesto_core_env(put_option).

%% @spec smtp_option() -> [] | error
%% @doc gen_smtpのsendオプションを取得する。
smtp_option() ->
	get_kesto_core_env(smtp_option).

%% @spec mail_from() -> string() | error
%% @doc メール通知の送信元メールアドレスを取得する。
mail_from() ->
	get_kesto_core_env(mail_from).

%% @spec ssh_client_connect_timeout() -> integer() | error
%% @doc SSH接続のタイムアウト(ミリ秒)を取得する。
ssh_client_connect_timeout() ->
	get_kesto_core_env(ssh_client_connect_timeout).

%% @spec expiration_date() -> integer() | error
%% @doc セッション有効期限を取得する。
expiration_date() ->
	get_kesto_core_env(expiration_date).

%% @private
get_kesto_core_env(Key) ->
	app_helper:get_env(kesto_core, Key).

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
	   fun put_option_test_case/0,
	   fun smtp_option_test_case/0,
	   fun mail_from_test_case/0,
	   fun ssh_client_connect_timeout_test_case/0,
	   fun expiration_date_test_case/0
	  ]
	}.

put_option_test_case() ->
	application:set_env(kesto_core, put_option, [{w, 2}, {dw, 1}, return_body]),
	?assertEqual([{w, 2}, {dw, 1}, return_body], put_option()).

smtp_option_test_case() ->
	application:set_env(kesto_core, smtp_option, [{relay, "smtp.test.co.jp"}, 
												  {username, "test"}, 
												  {password, "password"}, 
												  {port, 465}, 
												  {ssl, true}]),
	?assertEqual([{relay, "smtp.test.co.jp"}, 
				  {username, "test"}, 
				  {password, "password"}, 
				  {port, 465}, 
				  {ssl, true}], smtp_option()).

mail_from_test_case() ->
	application:set_env(kesto_core, mail_from, "test@test.co.jp"),
	?assertEqual("test@test.co.jp", mail_from()).

ssh_client_connect_timeout_test_case() ->
	application:set_env(kesto_core, ssh_client_connect_timeout, 30000),
	?assertEqual(30000, ssh_client_connect_timeout()).

expiration_date_test_case() ->
	application:set_env(kesto_core, expiration_date, 86400),
	?assertEqual(86400, expiration_date()).

setup() ->   
	application:load(kesto_core).

cleanup(_Pid) ->
	application:stop(kesto_core).

-endif.

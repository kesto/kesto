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
%% @doc ポーリング系監視用モジュール

-module(kesto_monitor_poller).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").

-export([
		 run/1
		]).

%% @spec check(term()) -> ok | {error, atom()} | {error, Error}
%% @doc 監視項目IDにより監視項目を取得するし、監視種別毎に監視を実行する。
run(ID) ->
	case kesto_monitor_conf:get(ID) of
		{ok, Conf} ->
			lager:debug("監視項目ID「~p」の監視を実行します。: ~p", [ID, Conf]),
			run_dispatcher(Conf);
		{error, Error} ->
			lager:error("不正な監視項目IDが渡されました。監視は実行されませんでした。: ~p", [ID]),
			{error, Error}
	end.

run_dispatcher(#monitor_conf{type=Type} = Conf) ->
	kesto_monitor_ping:run(Conf).

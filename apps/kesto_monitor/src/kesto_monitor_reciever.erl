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
%% @doc 受信系監視用モジュール

-module(kesto_monitor_reciever).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").
-include_lib("kesto_reciever/include/kesto_reciever.hrl").

-export([
		 check/1
		]).

%% @spec check(term()) -> ok | {error, atom()} | {error, Error}
%% @doc 受信データを監視項目と照合する。
check(Data) when is_record(Data, syslog) ->
	case kesto_monitor_conf:get_list(syslog) of
		{ok, List} ->
			loop_conf_list(List, Data);
		{error, Error} ->
			{error, Error}
	end;
check(_) ->
	{error, undefined}.

loop_conf_list([], _) -> ok;
loop_conf_list([Conf | T], Data) when is_record(Conf, monitor_conf) ->
	case check_conf(Conf, Data) of
		match ->
			ok;
		nomatch ->
			loop_conf_list(T, Data);
		{error, _} ->
			loop_conf_list(T, Data)
	end;
loop_conf_list([_ | T], Data) ->
	loop_conf_list(T, Data).

check_conf(Conf, Data) when is_record(Conf, monitor_conf) and is_record(Data, syslog) ->
	kesto_monitor_syslog:check(Conf, Data);
check_conf(_, _) ->
	{error, undefined}.

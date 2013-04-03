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
%% @doc 監視項目管理用モジュール

-module(kesto_monitor_conf).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").

-export([
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 get_list/1,
		 get_monitor_type/1
		]).

%% @spec put(#monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc 監視項目をRiakに登録する。
put(Conf) when is_record(Conf, monitor_conf) ->
	try riak_object:new(?KESTO_MONITOR_CONF_BUCKET, list_to_binary(Conf#monitor_conf.id), Conf) of
		Object ->
			lager:debug("監視項目用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("監視項目用riak_objectを登録しました。 : ~p", [Object]),
					kesto_scheduler_conf:put(Conf);
				{error, Error} ->
					lager:error("監視項目用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("監視項目用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
put(_) ->
	{error, undefined}.

%% @spec get(string()) -> #monitor_conf{} | {error, atom()} | {error, Error}
%% @doc Riakから監視項目IDを使って監視項目を取得する。
get(MonitorID) when is_list(MonitorID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_MONITOR_CONF_BUCKET, list_to_binary(MonitorID)) of
		{ok, Obj} ->
			lager:debug("監視項目用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("監視項目用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get(_) ->
	{error, undefined}.

%% @spec update(#monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc Riakに登録済みの監視項目を更新する。
update(Conf) when is_record(Conf, monitor_conf) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_MONITOR_CONF_BUCKET, list_to_binary(Conf#monitor_conf.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Conf) of
				NewObj ->
					lager:debug("監視項目用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新した監視項目用riak_objectを登録しました。 : ~p", [NewObj]),
							kesto_scheduler_conf:update(Conf);
						{error, Error} ->
							lager:error("更新した監視項目用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("監視項目用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("監視項目用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
update(_) ->
	{error, undefined}.

%% @spec delete(string()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakから監視項目IDを使って監視項目を削除する。
delete(MonitorID) when is_list(MonitorID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_MONITOR_CONF_BUCKET, list_to_binary(MonitorID)) of
		ok ->
			lager:debug("監視項目用riak_objectを削除しました。 : ~p", [MonitorID]),
			kesto_scheduler_conf:delete(MonitorID);
		{error, Error} ->
			lager:error("監視項目用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
delete(_) ->
	{error, undefined}.

%% @spec get_list(atom()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakから監視項目リストを取得する。
get_list(Type) when is_atom(Type) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Conf = riak_object:get_value(Obj),
				  case Conf#monitor_conf.type == Type of
					  true ->
						  [Conf];
					  false ->
						  []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_MONITOR_CONF_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("監視項目リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("監視項目リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get_list(_) ->
	{error, undefined}.

get_monitor_type(Conf) when is_record(Conf, monitor_conf) ->
	case Conf#monitor_conf.type of
		syslog -> recieve;
		ping -> polling
	end.

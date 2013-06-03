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
%% @doc スケジュール定義管理用モジュール

-module(kesto_scheduler_conf).

-compile([{parse_transform, lager_transform}]).

-include("kesto_scheduler.hrl").
-include_lib("kesto_monitor/include/kesto_monitor.hrl").

-export([
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 get_list/1,
		 get_id_list/1,
		 get_scheduler_type/1
		]).

%% @spec put(#scheduler_conf{} | #monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc スケジュール定義をRiakに登録する。
put(Conf) when is_record(Conf, monitor_conf) ->
	Sche = #scheduler_conf{id=Conf#monitor_conf.id, 
						   target_id=Conf#monitor_conf.id,
						   target_type=monitor,
						   type=get_scheduler_type(Conf), 
						   cycle=Conf#monitor_conf.cycle,
						   cron="", 
						   enabled=Conf#monitor_conf.enabled},
	put(Sche);
put(Conf) when is_record(Conf, scheduler_conf) ->
	try riak_object:new(?KESTO_SCHEDULER_CONF_BUCKET, list_to_binary(Conf#scheduler_conf.id), Conf) of
		Object ->
			lager:debug("スケジュール定義用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("スケジュール定義用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("スケジュール定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("スケジュール定義用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
put(_) ->
	{error, undefined}.

%% @spec get(string()) -> #scheduler_conf{} | {error, atom()} | {error, Error}
%% @doc RiakからスケジュールIDを使ってスケジュール定義を取得する。
get(SchedulerID) when is_list(SchedulerID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_SCHEDULER_CONF_BUCKET, list_to_binary(SchedulerID)) of
		{ok, Obj} ->
			lager:debug("スケジュール定義用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("スケジュール定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get(_) ->
	{error, undefined}.

%% @spec update(#scheduler_conf{} | #monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc Riakに登録済みのスケジュール定義を更新する。
update(Conf) when is_record(Conf, monitor_conf) ->
	Sche = #scheduler_conf{id=Conf#monitor_conf.id, 
						   target_id=Conf#monitor_conf.id,
						   target_type=monitor,
						   type=get_scheduler_type(Conf), 
						   cycle=Conf#monitor_conf.cycle,
						   cron="", 
						   enabled=Conf#monitor_conf.enabled},
	update(Sche);
update(Conf) when is_record(Conf, scheduler_conf) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_SCHEDULER_CONF_BUCKET, list_to_binary(Conf#scheduler_conf.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Conf) of
				NewObj ->
					lager:debug("スケジュール定義用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したスケジュール定義用riak_objectを登録しました。 : ~p", [NewObj]),
							ok;
						{error, Error} ->
							lager:error("更新したスケジュール定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("スケジュール定義用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("スケジュール定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
update(_) ->
	{error, undefined}.

%% @spec delete(string()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakからスケジュール定義IDを使ってスケジュール定義を削除する。
delete(SchedulerID) when is_list(SchedulerID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_SCHEDULER_CONF_BUCKET, list_to_binary(SchedulerID)) of
		ok ->
			lager:debug("スケジュール定義用riak_objectを削除しました。 : ~p", [SchedulerID]),
			ok;
		{error, Error} ->
			lager:error("スケジュール定義用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
delete(_) ->
	{error, undefined}.

%% @spec get_list(atom()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakからスケジュール定義リストを取得する。
get_list(Type) when is_atom(Type) ->
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Conf = riak_object:get_value(Obj),
				  case Conf#scheduler_conf.type == Type of
					  true ->
						  [Conf];
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_SCHEDULER_CONF_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("スケジュール定義リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("スケジュール定義リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get_list(_) ->
	{error, undefined}.

%% @spec get_id_list(atom()) -> {ok, [#scheduler_conf]} | {error, atom()} | {error, Error}
%% @doc Riakからスケジュール定義IDリストを取得する。
get_id_list(Type) when is_atom(Type) ->
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Conf = riak_object:get_value(Obj),
				  case Conf#scheduler_conf.type == Type of
					  true ->
						  case Conf#scheduler_conf.enabled of
							  true ->
								  [Conf#scheduler_conf.id];
							  false ->
								  []
						  end;
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_SCHEDULER_CONF_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("スケジュール定義リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("スケジュール定義リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get_id_list(_) ->
	{error, undefined}.

get_scheduler_type(Conf) when is_record(Conf, monitor_conf) ->
	case kesto_monitor_conf:get_monitor_type(Conf) of
		recieve ->
			none;
		polling ->
			simple
	end;
get_scheduler_type(Conf) when is_record(Conf, scheduler_conf) ->
	Conf#scheduler_conf.type.
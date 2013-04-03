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
%% @doc 通知定義用管理モジュール

-module(kesto_core_notify).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 get_list/1
		]).

%% @spec put(notify_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc 通知定義をRiakに登録する。
-spec put(notify_conf()) -> ok | {error, atom()} | {error, term()}.
put(Conf) ->
	try riak_object:new(?KESTO_CORE_NOTIFY_BUCKET, list_to_binary(Conf#notify_conf.id), Conf) of
		Object ->
			lager:debug("通知定義用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("通知定義用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("通知定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("通知定義用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(string()) -> {ok, notify_conf()} | {error, atom()} | {error, term()}.
%% @doc Riakから通知定義IDを使って通知定義を取得する。
-spec get(string()) -> {ok, notify_conf()} | {error, atom()} | {error, term()}.
get(ID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_NOTIFY_BUCKET, list_to_binary(ID)) of
		{ok, Obj} ->
			lager:debug("通知定義用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("通知定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update(notify_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みの通知定義を更新する。
-spec update(notify_conf()) -> ok | {error, atom()} | {error, term()}.
update(Conf) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_NOTIFY_BUCKET, list_to_binary(Conf#notify_conf.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Conf) of
				NewObj ->
					lager:debug("通知定義用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新した通知定義用riak_objectを登録しました。 : ~p", [NewObj]),
							ok;
						{error, Error} ->
							lager:error("更新した通知定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("通知定義用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("通知定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakから通知IDを使って通知定義を削除する。
-spec delete(string()) -> ok | {error, atom()} | {error, term()}.
delete(ID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_NOTIFY_BUCKET, list_to_binary(ID)) of
		ok ->
			lager:debug("通知定義用riak_objectを削除しました。 : ~p", [ID]),
			ok;
		{error, Error} ->
			lager:error("通知定義用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_list(true | false | all) -> {ok, [notify_conf()]} | {error, atom()} | {error, term()}.
%% @doc 通知定義リストを取得する。
-spec get_list(true | false | all) -> {ok, [notify_conf()]} | {error, atom()} | {error, term()}.
get_list(Enabled) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Conf = riak_object:get_value(Obj),
				  if
					  Enabled == Conf#notify_conf.enabled ->
						  [Conf];
					  Enabled == all ->
						  [Conf];
					  true ->
						  []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_NOTIFY_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("通知定義リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("通知定義リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

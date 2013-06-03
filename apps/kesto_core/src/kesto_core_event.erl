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
%% @doc イベント管理モジュール

-module(kesto_core_event).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 notify/2,
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 get_id/0,
		 get_list/0
		]).

%% @spec notify(string(), notify_info()) -> ok | {error, atom()} | {error, term()}.
%% @doc イベントをRiakに登録する。
-spec notify(string(), notify_info()) -> ok | {error, atom()} | {error, term()}.
notify(NotifyID, NotifyInfo) ->
	lager:info("~p", [{NotifyID, NotifyInfo}]),
	
	Conf = case kesto_core_notify:get(NotifyID) of
			   {ok, Obj1} -> Obj1;
			   {error, _Error1} -> notfound
		   end,
	case is_record(Conf, notify_conf) of
		true ->
			case is_record(Conf#notify_conf.conf, notify_event_conf) of
				true ->
					EventConf = Conf#notify_conf.conf,
					put_event(NotifyInfo, EventConf);
				false ->
					lager:error("通知定義[~s]にイベント通知定義が含まれていないため、イベント通知を行いません。", [NotifyID]),
					{error, notfound}
			end;
		false ->
			lager:error("通知定義[~s]が存在しないため、イベント通知を行いません。", [NotifyID]),
			{error, notfound}
	end.

%% @spec put_event(string(), notify_info(), notify_event_conf()) -> ok.
%% @doc 通知情報、イベント通知定義を元にイベントを登録する。
-spec put_event(notify_info(), notify_event_conf()) -> ok.
put_event(NotifyInfo, EventConf) ->
	case check_priority(NotifyInfo, EventConf) of
		{true, Check} ->
			Event = #event{id=kesto_core_event:get_id(), 
						   priority=NotifyInfo#notify_info.priority, 
						   timestamp=NotifyInfo#notify_info.timestamp,
						   timestamp_raw=NotifyInfo#notify_info.timestamp_raw,
						   monitor_type=NotifyInfo#notify_info.monitor_type,
						   monitor_id=NotifyInfo#notify_info.monitor_id,
						   facility_id=NotifyInfo#notify_info.facility_id, 
						   facility_name=NotifyInfo#notify_info.facility_name,
						   message=NotifyInfo#notify_info.message,
						   org_message=NotifyInfo#notify_info.org_message,
						   check=Check, 
						   comment=""},
			lager:info("イベント通知を実施。: ~p", [Event]),
			put(Event);
		false ->
			lager:debug("重要度別通知フラグにチェックが無いため、イベント通知を行いません。: ~p, ~p", [NotifyInfo, EventConf]),
			{error, nocheck}
	end.

%% @spec check_priority(notify_info(), notify_event_conf()) -> {true, true | false} | false.
%% @doc 通知情報、イベント通知定義を元にイベントの登録可否を確認する。
-spec check_priority(notify_info(), notify_event_conf()) -> {true, true | false} | false.
check_priority(_NotifyInfo=#notify_info{priority=Priority}, EventConf) ->
	case Priority of
		info ->
			case EventConf#notify_event_conf.info_enabled of
				true -> {true, EventConf#notify_event_conf.info_check};
				false -> false
			end;
		warning ->
			case EventConf#notify_event_conf.warning_enabled of
				true -> {true, EventConf#notify_event_conf.warning_check};
				false -> false
			end;
		error ->
			case EventConf#notify_event_conf.error_enabled of
				true -> {true, EventConf#notify_event_conf.error_check};
				false -> false
			end;
		unknown ->
			case EventConf#notify_event_conf.unknown_enabled of
				true -> {true, EventConf#notify_event_conf.unknown_check};
				false -> false
			end
	end.

%% @spec put(event()) -> ok | {error, atom()} | {error, term()}.
%% @doc イベントをRiakに登録する。
-spec put(event()) -> ok | {error, atom()} | {error, term()}.
put(Event) ->
	try riak_object:new(?KESTO_CORE_EVENT_BUCKET, list_to_binary(Event#event.id), Event) of
		Object ->
			lager:debug("イベント用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("イベント用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("イベント用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("イベント用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(string()) -> {ok, event()) | {error, atom()} | {error, term()}.
%% @doc RiakからイベントIDを使ってイベントを取得する。
-spec get(string()) -> {ok, event()} | {error, atom()} | {error, term()}.
get(EventID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_EVENT_BUCKET, list_to_binary(EventID)) of
		{ok, Obj} ->
			lager:debug("イベント用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("イベント用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update(event()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みのイベントを更新する。
-spec update(event()) -> ok | {error, atom()} | {error, term()}.
update(Event) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_EVENT_BUCKET, list_to_binary(Event#event.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Event) of
				NewObj ->
					lager:debug("イベント用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したイベント用riak_objectを登録しました。 : ~p", [NewObj]),
							ok;
						{error, Error} ->
							lager:error("更新したイベント用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("イベント用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("イベント用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string()) -> ok | {error, atom()} | {error, term()}.
%% @doc RiakからイベントIDを使ってイベントを削除する。
-spec delete(string()) -> ok | {error, atom()} | {error, term()}.
delete(EventID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_EVENT_BUCKET, list_to_binary(EventID)) of
		ok ->
			lager:debug("イベント用riak_objectを削除しました。 : ~p", [EventID]),
			ok;
		{error, Error} ->
			lager:error("イベント用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_id() -> string().
%% @doc イベントIDを生成する。
-spec get_id() -> string().
get_id() ->
	{MegaSecs, Secs, MicroSecs} = now(),
	ID = string:join([integer_to_list(MegaSecs), 
					  integer_to_list(Secs), 
					  integer_to_list(MicroSecs)], "_"),
	lager:debug("イベントIDを生成しました。 : ~s", [ID]),
	ID.

%% @spec get_list() -> {ok, [event()]} | {error, atom()} | {error, term()}.
%% @doc イベントリストを取得する。
-spec get_list() -> {ok, [event()]} | {error, atom()} | {error, term()}.
get_list() ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Event = riak_object:get_value(Obj),
				  [Event]
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_EVENT_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("イベントリストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("イベントリストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

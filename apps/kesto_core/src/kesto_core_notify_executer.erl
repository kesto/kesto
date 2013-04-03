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
%% @doc 通知管理モジュール

-module(kesto_core_notify_executer).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 execute/1
		]).

-export([
		 put/2
		]).

%% @spec execute(notify_info()) -> ok | {error, atom()} | {error, term()}.
%% @doc 通知情報を通知定義及び通知履歴を元に通知する。
-spec execute(notify_info()) -> ok | {error, atom()} | {error, term()}.
execute(Info=#notify_info{id=ID}) ->
	execute(ID, Info, []).

%% @spec execute([string()], notify_info(), [{string(), notify_info()}]) -> {ok, [{string(), notify_info()}]} | {error, atom()} | {error, term()}.
%% @doc 通知情報を通知ID毎に通知する。
-spec execute([string()], notify_info(), [{string(), notify_info()}]) -> {ok, [{string(), notify_info()}]} | {error, atom()} | {error, term()}.
execute([], _Info, OutputList) ->
	{ok, OutputList};
execute([ID | T], Info, OutputList) ->
	case get_conf_and_history(ID, Info) of
		{ok, {Conf, History}} ->
			lager:debug("通知定義と通知履歴を取得。 : ~p, ~p", [Conf, History]),
			case History#notify_history.initial_notify of
				true ->
					case check_renotify(Conf, History, Info) of
						true ->
							NewHistory = 
								History#notify_history{timestamp=Info#notify_info.timestamp,
													   counter=1},
							kesto_core_notify_executer:put(ID, NewHistory),
							lager:info("重要度変化後の再通知を実施。 : ~p, ~p, ~p", [Conf, Info, NewHistory]),
							notify(ID, Info, Conf),
							execute(T, Info, [{ID, Info} | OutputList]);
						false ->
							NewHistory = 
								History#notify_history{counter=History#notify_history.counter + 1},
							lager:info("通知履歴のカウンターに1を足す。 : ~p", [NewHistory]),
							kesto_core_notify_executer:put(ID, NewHistory),
							execute(T, Info, OutputList)
					end;
				false ->
					case check_initial_count(Conf, History) of
						true ->
							NewHistory = 
								History#notify_history{timestamp=Info#notify_info.timestamp,
													   initial_notify=true,
													   counter=1},
							lager:info("重要度変化後の初期通知を実施。 : ~p, ~p, ~p", [Conf, Info, NewHistory]),
							kesto_core_notify_executer:put(ID, NewHistory),
							notify(ID, Info, Conf),
							execute(T, Info, [{ID, Info} | OutputList]);
						false ->
							NewHistory = 
								History#notify_history{counter=History#notify_history.counter + 1},
							lager:debug("通知履歴のカウンターに1を足す。 : ~p", [NewHistory]),
							kesto_core_notify_executer:put(ID, NewHistory),
							execute(T, Info, OutputList)
					end
			end;
		{error, notfound} ->
			lager:debug("通知定義を取得できませんでした。 : ~p", [ID]),
			execute(T, Info, OutputList)
	end.

-spec get_conf_and_history(string(), notify_info()) -> {ok, {notify_conf(), notify_history()}} | {error, atom()} | {error, term()}.
get_conf_and_history(ID, Info) ->
	case kesto_core_notify:get(ID) of
		{ok, Conf} ->
			case Conf#notify_conf.enabled of
				true ->
					case get(ID, Info) of
						{ok, History} ->
							case check_priority(Info, History) of
								true ->
									lager:info("重要度に変化なし、何もしない。 : ~p", [History]),
									{ok, {Conf, History}};
								false ->
									NewHistory = 
										History#notify_history{priority=Info#notify_info.priority,
															   timestamp=Info#notify_info.timestamp,
															   initial_notify=false,
															   counter=1},
									lager:info("重要度が変化したため、通知履歴を初期化する。 : ~p", [NewHistory]),
									kesto_core_notify_executer:put(ID, NewHistory),
									{ok, {Conf, NewHistory}}
							end;
						{error, notfound} ->
							History = #notify_history{id=ID, 
													  priority=Info#notify_info.priority,
													  monitor_type=Info#notify_info.monitor_type, 
													  monitor_id=Info#notify_info.monitor_id, 
													  facility_id=Info#notify_info.facility_id, 
													  timestamp=Info#notify_info.timestamp, 
													  initial_notify=false,
													  counter=1},
							lager:info("通知履歴が存在しないため、通知履歴を作成する。 : ~p", [History]),
							kesto_core_notify_executer:put(ID, History),
							{ok, {Conf, History}};
						{error, _Error} ->
							lager:error("通知履歴が取得できなかったため、通知情報を破棄します。 : ~p", [Info]),
							{error, notfound}
					end;
				false ->
					lager:error("通知定義が無効のため、通知情報を破棄します。 : ~p", [Info]),
					{error, notfound}
			end;
		{error, _Error} ->
			lager:error("通知定義を取得できなかったため、通知情報を破棄します。 : ~p", [Info]),
			{error, notfound}
	end.

%% @spec check_priority(notify_info(), notify_history()) -> {ok, notify_history()}.
%% @doc 重要度の変化を確認する。
-spec check_priority(notify_info(), notify_history()) -> {ok, notify_history()}.
check_priority(_Info=#notify_info{priority=Priority1}, 
			   _History=#notify_history{priority=Priority2}) ->
	Priority1 == Priority2.

%% @spec check_initial_count(notify_conf(), notify_history()) -> true | false.
%% @doc 重要度変化後の初回通知の回数を確認する。
-spec check_initial_count(notify_conf(), notify_history()) -> true | false.
check_initial_count(_Conf=#notify_conf{initial_count=Count}, 
					_History=#notify_history{counter=Counter}) ->
	Counter >= Count.

%% @spec check_renotify(notify_conf(), notify_history(), notify_info()) -> true | false.
%% @doc 重要度変化後の再通知の可否を取得する。
-spec check_renotify(notify_conf(), notify_history(), notify_info()) -> true | false.
check_renotify(_Conf=#notify_conf{renotify_type=Type,
								  renotify_value=Value}, 
			   _History=#notify_history{timestamp=Last,
										counter=Counter},
			   _Info=#notify_info{timestamp=Now}) ->
	case Type of
		all ->
			true;
		period ->
			Next = calendar:gregorian_seconds_to_datetime(
					 calendar:datetime_to_gregorian_seconds(Last) + Value),
			Now >= Next;
		count ->
			Counter >= Value;
		ignore ->
			false
	end.

%% @spec notify(string(), notify_info(), notify_conf()) -> ok.
%% @doc 通知を実施する。
-spec notify(string(), notify_info(), notify_conf()) -> ok.
notify(NotifyID, NotifyInfo, Conf) ->
	DocIdx = riak_core_util:chash_key({<<"notify">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	if
		is_record(Conf#notify_conf.conf, notify_event_conf) ->
			kesto_core_event_vnode:notify(IdxNode, NotifyID, NotifyInfo);
		is_record(Conf#notify_conf.conf, notify_mail_conf) ->
			kesto_core_mail_vnode:notify(IdxNode, NotifyID, NotifyInfo);
		is_record(Conf#notify_conf.conf, notify_command_conf) ->
			kesto_core_command_vnode:notify(IdxNode, NotifyID, NotifyInfo);
		true ->
			ok
	end.

%% @spec put(notify()) -> ok | {error, atom()} | {error, term()}.
%% @doc 通知履歴をRiakに登録する。
-spec put(string(), notify_history()) -> ok | {error, atom()} | {error, term()}.
put(ID, NotifyHistory) ->
	Key = get_key(ID, NotifyHistory),
	try riak_object:new(?KESTO_CORE_NOTIFY_HISTORY_BUCKET, list_to_binary(Key), NotifyHistory) of
		Object ->
			lager:debug("通知履歴用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("通知履歴用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("通知履歴用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:debug("通知履歴用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(string(), notify_history() | notify_info()) -> {ok, notify_history()} | {error, atom()} | {error, term()}.
%% @doc Riakから通知履歴IDを使って通知履歴を取得する。
-spec get(string(), notify_history() | notify_info()) -> {ok, notify_history()} | {error, atom()} | {error, term()}.
get(ID, Notify) ->
	Key = get_key(ID, Notify),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_NOTIFY_HISTORY_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			lager:debug("通知履歴用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:debug("通知履歴用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update(notify()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みの通知履歴を更新する。
-spec update(string(), notify_history()) -> ok | {error, atom()} | {error, term()}.
update(ID, NotifyHistory) ->
	Key = get_key(ID, NotifyHistory),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_NOTIFY_HISTORY_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, NotifyHistory) of
				NewObj ->
					lager:debug("通知履歴用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新した通知履歴用riak_objectを登録しました。 : ~p", [NewObj]),
							ok;
						{error, Error} ->
							lager:debug("更新した通知履歴用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:debug("通知履歴用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:debug("通知履歴用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string(), notify_history() | notify_info()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakから通知履歴IDを使って通知履歴を削除する。
-spec delete(string(), notify_history() | notify_info()) -> ok | {error, atom()} | {error, term()}.
delete(ID, Notify) ->
	Key = get_key(ID, Notify),
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_NOTIFY_HISTORY_BUCKET, list_to_binary(Key)) of
		ok ->
			lager:debug("通知履歴用riak_objectを削除しました。 : ~p", [Key]),
			ok;
		{error, Error} ->
			lager:debug("通知履歴用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_key(string(), notify_history() | notify_info()) -> string() | {error, atom()} | {error, term()}.
%% @doc 通知履歴のキーを取得する。
-spec get_key(string(), notify_history() | notify_info()) -> string() | {error, atom()} | {error, term()}.
get_key(ID, _NotifyHistory=#notify_history{monitor_type=MonitorType,
										   monitor_id=MonitorID,
										   facility_id=FacilityID}) ->
	Key = string:join([ID,
					   FacilityID,
					   erlang:atom_to_list(MonitorType),
					   MonitorID], ";"),
	lager:debug("~s", [Key]),
	Key;
get_key(ID, _NotifyInfo=#notify_info{monitor_type=MonitorType,
									 monitor_id=MonitorID,
									 facility_id=FacilityID}) ->
	Key = string:join([ID,
					   FacilityID,
					   erlang:atom_to_list(MonitorType),
					   MonitorID], ";"),
	lager:debug("~s", [Key]),
	Key.

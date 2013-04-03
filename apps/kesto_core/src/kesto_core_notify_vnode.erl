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
%% @doc 通知管理用vnode

-module(kesto_core_notify_vnode).
-behaviour(riak_core_vnode).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
		 init/1,
		 terminate/2,
		 handle_command/3,
		 is_empty/1,
		 delete/1,
		 handle_handoff_command/3,
		 handoff_starting/2,
		 handoff_cancelled/1,
		 handoff_finished/2,
		 handle_handoff_data/2,
		 encode_handoff_item/2,
		 handle_coverage/4,
		 handle_exit/3]).

-export([
		 execute/2,
		 put/2,
		 get/2,
		 update/2,
		 delete/2,
		 get_list/2
		]).

-record(state, {partition}).

-define(MASTER, kesto_core_notify_vnode_master).

start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, #state { partition=Partition }}.

%% @spec execute(term(), notify_info()) -> ok | {error, atom()} | {error, term()}.
%% @doc 通知情報を通知する。
-spec execute(term(), notify_info()) -> ok | {error, atom()} | {error, term()}.
execute(IdxNode, Info) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{execute, Info},
										?MASTER).

%% @spec put(term(), notify_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc 通知定義をRiakに登録する。
-spec put(term(), notify_conf()) -> ok | {error, atom()} | {error, term()}.
put(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{put, Conf},
										?MASTER).

%% @spec get(term(), string()) -> notify_conf() | {error, atom()} | {error, term()}.
%% @doc Riakから通知定義IDを使って通知定義を取得する。
-spec get(term(), string()) -> notify_conf() | {error, atom()} | {error, term()}.
get(IdxNode, NotifyID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get, NotifyID},
										?MASTER).

%% @spec update(term(), notify_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みの通知定義を更新する。
-spec update(term(), notify_conf()) -> ok | {error, atom()} | {error, term()}.
update(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{update, Conf},
										?MASTER).

%% @spec delete(term(), string()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakから通知定義IDを使って通知定義を削除する。
-spec delete(term(), string()) -> ok | {error, atom()} | {error, term()}.
delete(IdxNode, NotifyID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{delete, NotifyID},
										?MASTER).

%% @spec get_list(term(), true | false | all) -> [notify_conf()] | {error, atom()} | {error, term()}.
%% @doc 通知定義リストを取得する。
-spec get_list(term(), true | false | all) -> [notify_conf()] | {error, atom()} | {error, term()}.
get_list(IdxNode, Enabled) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get_list, Enabled},
										?MASTER).

handle_command({execute, Info}, _Sender, State) ->
	lager:debug("~p", [Info]),
	case kesto_core_notify_executer:execute(Info) of
		{ok, _List} ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({put, Conf}, _Sender, State) ->
	lager:debug("~p", [Conf]),
	case kesto_core_notify:put(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get, NotifyID}, _Sender, State) ->
	lager:debug("~s", [NotifyID]),
	case kesto_core_notify:get(NotifyID) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({update, Conf}, _Sender, State) ->
	lager:debug("~p", [Conf]),
	case kesto_core_notify:update(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			lager:error("update event riak_object : ~p", [Error]),
			{reply, {error, Error}, State}
	end;

handle_command({delete, NotifyID}, _Sender, State) ->
	lager:debug("~s", [NotifyID]),
	case kesto_core_notify:delete(NotifyID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get_list, Enabled}, _Sender, State) ->
	lager:debug("~p", [Enabled]),
	case kesto_core_notify:get_list(Enabled) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end.

handle_handoff_command(_Message, _Sender, State) ->
	{noreply, State}.

handoff_starting(_TargetNode, State) ->
	{true, State}.

handoff_cancelled(State) ->
	{ok, State}.

handoff_finished(_TargetNode, State) ->
	{ok, State}.

handle_handoff_data(_Data, State) ->
	{reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
	<<>>.

is_empty(State) ->
	{true, State}.

delete(State) ->
	{ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
	{stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
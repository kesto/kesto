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
%% @doc 監視項目管理用vnode

-module(kesto_monitor_vnode).
-behaviour(riak_core_vnode).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").
-include_lib("kesto_reciever/include/kesto_reciever.hrl").
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
		 put_conf/2,
		 get_conf/2,
		 update_conf/2,
		 delete_conf/2,
		 get_conf_list/2,
		 check/2,
		 run/2
		]).

-record(state, {partition}).

-define(MASTER, kesto_monitor_vnode_master).

start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, #state { partition=Partition }}.

%% @spec put_conf(term(), #monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc 監視項目をRiakに登録する。
put_conf(IdxNode, Conf) when is_record(Conf, monitor_conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{put_conf, Conf},
										?MASTER);
put_conf(_, _) ->
	{error, undefined}.

%% @spec get_conf(term(), string()) -> #monitor_conf{} | {error, atom()} | {error, Error}
%% @doc Riakから監視項目IDを使って監視項目を取得する。
get_conf(IdxNode, MonitorID) when is_list(MonitorID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get_conf, MonitorID},
										?MASTER);
get_conf(_, _) ->
	{error, undefined}.

%% @spec update_conf(term(), #monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc Riakに登録済みの監視項目を更新する。
update_conf(IdxNode, Conf) when is_record(Conf, monitor_conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{update_conf, Conf},
										?MASTER);
update_conf(_, _) ->
	{error, undefined}.

%% @spec delete_conf(term(), string()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakから監視項目IDを使って監視項目を削除する。
delete_conf(IdxNode, MonitorID) when is_list(MonitorID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{delete_conf, MonitorID},
										?MASTER);
delete_conf(_, _) ->
	{error, undefined}.

%% @spec get_conf_list(term(), atom()) -> ok | {error, atom()} | {error, Error}
%% @doc Riakから監視項目リストを取得する。
get_conf_list(IdxNode, MonitorType) when is_atom(MonitorType) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get_conf_list, MonitorType},
										?MASTER);
get_conf_list(_, _) ->
	{error, undefined}.

%% @spec check(term(), term()) -> ok | {error, atom()} | {error, Error}
%% @doc 受信データを監視項目と照合する。
check(IdxNode, Data) ->
	riak_core_vnode_master:command(IdxNode,
								   {check, Data},
								   ?MASTER).

%% @spec check(term(), string()) -> ok | {error, atom()} | {error, Error}
%% @doc 監視項目の監視を実施する。
run(IdxNode, MonitorID) ->
	riak_core_vnode_master:command(IdxNode,
								   {run, MonitorID},
								   ?MASTER).

handle_command({put_conf, Conf}, _Sender, State) ->
	lager:debug("Conf : ~p", [Conf]),
	case kesto_monitor_conf:put(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get_conf, MonitorID}, _Sender, State) ->
	lager:debug("MonitorID : ~s", [MonitorID]),
	case kesto_monitor_conf:get(MonitorID) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({update_conf, Conf}, _Sender, State) ->
	lager:debug("Conf : ~p", [Conf]),
	case kesto_monitor_conf:update(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({delete_conf, MonitorID}, _Sender, State) ->
	lager:debug("MonitorID : ~s", [MonitorID]),
	case kesto_monitor_conf:delete(MonitorID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get_conf_list, MonitorType}, _Sender, State) ->
	lager:debug("MonitorType : ~p", [MonitorType]),
	case kesto_monitor_conf:get_list(MonitorType) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({check, Data}, _Sender, State) ->
	lager:debug("Data : ~p", [Data]),
	case kesto_monitor_reciever:check(Data) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({run, MonitorID}, _Sender, State) ->
	lager:debug("MonitorID : ~p", [MonitorID]),
	case kesto_monitor_poller:run(MonitorID) of
		ok ->
			{reply, ok, State};
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

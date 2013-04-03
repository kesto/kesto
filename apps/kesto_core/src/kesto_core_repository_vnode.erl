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
%% @doc リポジトリ管理用vnode

-module(kesto_core_repository_vnode).
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
		 put/2,
		 get/2,
		 update/2,
		 delete/2,
		 assign/3,
		 unassign/3
		]).

-record(state, {partition}).

-define(MASTER, kesto_core_repository_vnode_master).

start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, #state { partition=Partition }}.

%% @spec put(term(), #node{} | #scope{}) -> ok | {error, atom()} | {error, Error}
%% @doc ノード/スコープをRiakに登録する。
put(IdxNode, Facility) when is_record(Facility, node) or is_record(Facility, scope) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{put, Facility},
										?MASTER);
put(_, _) ->
	{error, undefined}.

%% @spec get(term(), string()) -> #node{} | #scope{} | {error, atom()} | {error, Error}
%% @doc RiakからファシリティIDを使ってノード/スコープを取得する。
get(IdxNode, FacilityID) when is_list(FacilityID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get, FacilityID},
										?MASTER);
get(_, _) ->
	{error, undefined}.

%% @spec update(term(), #node{} | #scope{}) -> ok | {error, atom()} | {error, Error}
%% @doc Riakに登録済みのノード/スコープを更新する。
update(IdxNode, Facility) when is_record(Facility, node) or is_record(Facility, scope) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{update, Facility},
										?MASTER);
update(_, _) ->
	{error, undefined}.

%% @spec delete(term(), string()) -> ok | {error, atom()} | {error, Error}
%% @doc RiakからファシリティIDを使ってノード/スコープを削除する。
delete(IdxNode, FacilityID) when is_list(FacilityID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{delete, FacilityID},
										?MASTER);
delete(_, _) ->
	{error, undefined}.

%% @spec assign(term(), string(), [string()]) -> ok | {error, atom()} | {error, Error}
%% @doc スコープにノードを割り当て、Riakに登録/更新する。
assign(IdxNode, FacilityID, FacilityIDList) when is_list(FacilityID) and is_list(FacilityIDList) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{assign, FacilityID, FacilityIDList},
										?MASTER);
assign(_, _, _) ->
	{error, undefined}.

%% @spec unassign(term(), string(), [string()]) -> ok | {error, atom()} | {error, Error}
%% @doc スコープからノードの割り当てを解除し、Riakに登録/更新する。
unassign(IdxNode, FacilityID, FacilityIDList) when is_list(FacilityID) and is_list(FacilityIDList) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{unassign, FacilityID, FacilityIDList},
										?MASTER);
unassign(_, _, _) ->
	{error, undefined}.

handle_command({put, Facility}, _Sender, State) ->
	lager:debug("put node : ~p", [Facility]),
	case kesto_core_repository:put(Facility) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get, FacilityID}, _Sender, State) ->
	lager:debug("get node : ~s", [FacilityID]),
	case kesto_core_repository:get(FacilityID) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({update, Facility}, _Sender, State) ->
	lager:debug("update node : ~p", [Facility]),
	case kesto_core_repository:update(Facility) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({delete, FacilityID}, _Sender, State) ->
	lager:debug("delete node : ~s", [FacilityID]),
	case kesto_core_repository:delete(FacilityID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({assign, FacilityID, FacilityIDList}, _Sender, State) ->
	lager:debug("assign node : ~s, ~p", [FacilityID, FacilityIDList]),
	case kesto_core_repository:assign(FacilityID, FacilityIDList) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({unassign, FacilityID, FacilityIDList}, _Sender, State) ->
	lager:debug("unassign node : ~s, ~p", [FacilityID, FacilityIDList]),
	case kesto_core_repository:unassign(FacilityID, FacilityIDList) of
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

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

-module(kesto_job_vnode).

-behaviour(riak_core_vnode).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-compile([{parse_transform, lager_transform}]).

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
		 run/3,
		 operate_session/5,
		 operate_session/7,
		 operate_session/8,
		 operate/4,
		 operate/6,
		 operate/7,
		 put_conf/2,
		 get_conf/3,
		 update_conf/2,
		 delete_conf/3,
		 get_conf_list/2
		]).

-record(state, {partition}).

-define(MASTER, kesto_job_vnode_master).

%% API
start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, #state { partition=Partition }}.

%% @doc ジョブを実行する。
-spec run(term(), string(), string()) -> {ok, session_id()} | {error, atom()} | {error, term()}.
run(IdxNode, GroupID, JobID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{run, GroupID, JobID},
										?MASTER).

%% @doc セッションを操作する。
-spec operate_session(term(), session_id(), operation_id(), job_operation(), job_end_value()) -> 
		  ok | {error, atom()} | {error, term()}.
operate_session(IdxNode, SessionID, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, OperationID, Operation, EndValue}]),
	riak_core_vnode_master:command(IdxNode,
								   {operate_session, {SessionID}, OperationID, Operation, EndValue},
								   ?MASTER).
-spec operate_session(term(), session_id(), group_id(), job_id(), operation_id(), job_operation(), job_end_value()) -> 
		  ok | {error, atom()} | {error, term()}. 
operate_session(IdxNode, SessionID, GroupID, JobID, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, OperationID, Operation, EndValue}]),
	riak_core_vnode_master:command(IdxNode,
								   {operate_session, {SessionID, GroupID, JobID}, OperationID, Operation, EndValue},
								   ?MASTER).
-spec operate_session(term(), session_id(), group_id(), job_id(), facility_id(), operation_id(), job_operation(), job_end_value()) -> 
		  ok | {error, atom()} | {error, term()}.
operate_session(IdxNode, SessionID, GroupID, JobID, FacilityID, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID, OperationID, Operation, EndValue}]),
	riak_core_vnode_master:command(IdxNode,
								   {operate_session, {SessionID, GroupID, JobID, FacilityID}, OperationID, Operation, EndValue},
								   ?MASTER).

%% @doc セッションを操作する。
-spec operate(term(), session_id(), job_operation(), job_end_value()) -> 
		  {ok, operation_id()} | {error, atom()} | {error, term()}.
operate(IdxNode, SessionID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, Operation, EndValue}]),
	riak_core_vnode_master:sync_command(IdxNode,
										{operate, {SessionID}, Operation, EndValue},
										?MASTER).
-spec operate(term(), session_id(), group_id(), job_id(), job_operation(), job_end_value()) -> 
		  {ok, operation_id()} | {error, atom()} | {error, term()}.
operate(IdxNode, SessionID, GroupID, JobID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, Operation, EndValue}]),
	riak_core_vnode_master:sync_command(IdxNode,
										{operate, {SessionID, GroupID, JobID}, Operation, EndValue},
										?MASTER).
-spec operate(term(), session_id(), group_id(), job_id(), facility_id(), job_operation(), job_end_value()) -> 
		  {ok, operation_id()} | {error, atom()} | {error, term()}.
operate(IdxNode, SessionID, GroupID, JobID, FacilityID, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID, Operation, EndValue}]),
	riak_core_vnode_master:sync_command(IdxNode,
										{operate, {SessionID, GroupID, JobID, FacilityID}, Operation, EndValue},
										?MASTER).

%% @doc ジョブ定義をRiakに登録する。
-spec put_conf(term(), job_conf()) -> ok | {error, atom()} | {error, term()}.
put_conf(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{put_conf, Conf},
										?MASTER).

%% @doc Riakからジョブ定義IDを使ってジョブ定義を取得する。
-spec get_conf(term(), string(), string()) -> job_conf() | {error, atom()} | {error, term()}.
get_conf(IdxNode, Group, JobID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get_conf, Group, JobID},
										?MASTER).

%% @doc Riakに登録済みのジョブ定義を更新する。
-spec update_conf(term(), job_conf()) -> ok | {error, atom()} | {error, term()}.
update_conf(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{update_conf, Conf},
										?MASTER).

%% @doc Riakからジョブ定義IDを使ってジョブ定義を削除する。
-spec delete_conf(term(), string(), string()) -> ok | {error, atom()} | {error, term()}.
delete_conf(IdxNode, Group, JobID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{delete_conf, Group, JobID},
										?MASTER).

%% @doc Riakからジョブ定義リストを取得する。
-spec get_conf_list(term(), job_type()) -> [job_conf()] | {error, atom()} | {error, term()}.
get_conf_list(IdxNode, Type) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get_conf_list, Type},
										?MASTER).

handle_command({run, GroupID, JobID}, _Sender, State) ->
	lager:debug("~p", [{GroupID, JobID}]),
	case kesto_job_executer:run(GroupID, JobID) of
		{ok, SessionID} ->
			{reply, {ok, SessionID}, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate_session, {SessionID}, OperationID, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID}, OperationID, Operation, EndValue}]),
	case kesto_job_session_operator:operate({SessionID}, OperationID, Operation, EndValue) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate_session, {SessionID, GroupID, JobID}, OperationID, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID, GroupID, JobID}, OperationID, Operation, EndValue}]),
	case kesto_job_session_operator:operate({SessionID, GroupID, JobID}, OperationID, Operation, EndValue) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate_session, {SessionID, GroupID, JobID, FacilityID}, OperationID, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID, GroupID, JobID, FacilityID}, OperationID, Operation, EndValue}]),
	case kesto_job_session_operator:operate({SessionID, GroupID, JobID, FacilityID}, OperationID, Operation, EndValue) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate, {SessionID}, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID}, Operation, EndValue}]),
	case kesto_job_executer:operate({SessionID}, Operation, EndValue) of
		{ok, OperationID} ->
			{reply, {ok, OperationID}, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate, {SessionID, GroupID, JobID}, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID, GroupID, JobID}, Operation, EndValue}]),
	case kesto_job_executer:operate({SessionID, GroupID, JobID}, Operation, EndValue) of
		{ok, OperationID} ->
			{reply, {ok, OperationID}, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({operate, {SessionID, GroupID, JobID, FacilityID}, Operation, EndValue}, _Sender, State) ->
	lager:debug("~p", [{{SessionID, GroupID, JobID, FacilityID}, Operation, EndValue}]),
	case kesto_job_executer:operate({SessionID, GroupID, JobID, FacilityID}, Operation, EndValue) of
		{ok, OperationID} ->
			{reply, {ok, OperationID}, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({put_conf, Conf}, _Sender, State) ->
	lager:debug("~p", [Conf]),
	case kesto_job_conf:put(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get_conf, Group, JobID}, _Sender, State) ->
	lager:debug("~s", [{Group, JobID}]),
	case kesto_job_conf:get(Group, JobID) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({update_conf, Conf}, _Sender, State) ->
	lager:debug("~p", [Conf]),
	case kesto_job_conf:update(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({delete_conf, Group, JobID}, _Sender, State) ->
	lager:debug("~s", [{Group, JobID}]),
	case kesto_job_conf:delete(Group, JobID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get_conf_list, Type}, _Sender, State) ->
	case kesto_job_conf:get_list(Type) of
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

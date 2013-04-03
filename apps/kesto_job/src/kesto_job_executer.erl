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
%% @doc ジョブ実行モジュール

-module(kesto_job_executer).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

%% API
-export([
		 run/2,
		 operate/3
		]).

%% @doc セッション情報を作成し、ジョブを実行する。
-spec run(group_id(), job_id()) -> {ok, string()} | {error, term()}.
run(GroupID, JobID) ->
	lager:debug("~p", [{GroupID, JobID}]),
	try kesto_job_conf:get_relation_list(GroupID, JobID) of
		ConfList ->
			SessionID = make_session_id(),
			case make_session(SessionID, GroupID, JobID) of
				{ok, _Session} ->
					case make_session_job(SessionID, ConfList, []) of
						{ok, _SessionJobList} ->
							case make_session_node(SessionID, ConfList, []) of
								{ok, _SessionNodeList} ->
									Preflist = kesto_job_session_util:get_preflist(),
									kesto_job_vnode:operate_session(Preflist, 
																	SessionID, 
																	undefined, 
																	start, 0),
									{ok, SessionID};
								{error, _Error} -> {error, _Error}
							end;
						{error, _Error} -> {error, _Error}
					end;
				{error, _Error} -> {error, _Error}
			end
	catch
		_:_Error ->
			lager:error("ジョブ定義の取得に失敗しました。~p", [_Error]),
			{error, _Error}
	end.

%% @doc セッション情報を作成する。
-spec make_session(session_id(), group_id(), job_id()) -> {ok, job_session()} | {error, term()}.
make_session(SessionID, GroupID, JobID) ->
	lager:debug("~p", [{SessionID, GroupID, JobID}]),
	Session = #job_session{id=SessionID, 
						   group_id=GroupID, 
						   job_id=JobID},
	case kesto_job_session_info:put(Session) of
		ok ->
			{ok, Session};
		{error, _Error} ->
			{error, _Error}
	end.

%% @doc セッションジョブ情報を作成する。
-spec make_session_job(session_id(), 
					   [{{group_id(), group_id()}, job_conf()}], 
					   [job_session_job()]) -> 
		  {ok, [job_session_job()]} | {error, term()}.
make_session_job(_SessionID, [], SessionJobList) ->
	{ok, SessionJobList};
make_session_job(SessionID, [{{GroupID, JobID}, Conf} | T], SessionJobList) ->
	SessionJob = #job_session_job{id=SessionID,
								  group_id=GroupID,
								  job_id=JobID,
								  conf=Conf},
	case kesto_job_session_info:put(SessionJob) of
		ok ->
			make_session_job(SessionID, T, [SessionJob | SessionJobList]);
		{error, _Error} ->
			{error, _Error}
	end.

%% @doc セッションノード情報を作成する。
-spec make_session_node(session_id(), 
						[{{group_id(), job_id()}, job_conf()}], 
						[job_session_node()]) -> 
		  {ok, [job_session_node()]} | {error, term()}.
make_session_node(_SessionID, [], SessionNodeList) ->
	{ok, SessionNodeList};
make_session_node(SessionID, [{{GroupID, JobID}, Conf} | T], SessionNodeList) ->
	case Conf#job_conf.type == job of
		true ->
			CmdConf = Conf#job_conf.command,
			FacilityID = CmdConf#job_conf_command.cmd_facility_id,
			case kesto_core_repository:get_node_facility_id_list(FacilityID, true) of
				{ok, FacilityIDList} -> 
					case make_session_node(SessionID, GroupID, JobID, FacilityIDList, SessionNodeList) of
						{ok, NewSessionNodeList} ->
							make_session_node(SessionID, T, NewSessionNodeList);
						{error, Error} -> 
							{error, Error}
					end;
				{error, Error} -> 
					{error, Error}
			end;
		false ->
			make_session_node(SessionID, T, SessionNodeList)
	end.
-spec make_session_node(session_id(), 
						group_id(), 
						job_id(), 
						[facility_id()], 
						[job_session_node()]) -> 
		  {ok, [job_session_node()]} | {error, term()}.
make_session_node(_SessionID, _GroupID, _JobID, [], SessionNodeList) ->
	{ok, SessionNodeList};
make_session_node(SessionID, GroupID, JobID, [FacilityID | T], SessionNodeList) ->
	case kesto_core_repository:get(FacilityID) of
		{ok, Facility} ->
			SessionNode = #job_session_node{id=SessionID,
											group_id=GroupID,
											job_id=JobID,
											facility=Facility}, 
			case kesto_job_session_info:put(SessionNode) of
				ok ->
					make_session_node(SessionID, GroupID, JobID, T, [SessionNode | SessionNodeList]);
				{error, _Error} ->
					{error, _Error}
			end;
		{error, _Error} ->
			{error, _Error}
	end.

%% @doc セッションIDを生成する。
-spec make_session_id() -> session_id().
make_session_id() -> 
	make_session_id(0).
-spec make_session_id(integer()) -> session_id().
make_session_id(Count) -> 
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	ID = lists:concat([erlang:integer_to_list(Year), 
					   string:right(erlang:integer_to_list(Month), 2, $0), 
					   string:right(erlang:integer_to_list(Day), 2, $0), 
					   "_", 
					   string:right(erlang:integer_to_list(Hour), 2, $0), 
					   string:right(erlang:integer_to_list(Minute), 2, $0), 
					   string:right(erlang:integer_to_list(Second), 2, $0), 
					   "_", 
					   string:right(erlang:integer_to_list(Count), 3, $0)]),
	case kesto_job_session_info:get(ID) of
		{ok, _Session} ->
			make_session_id(Count + 1);
		{error, notfound} ->
			ID
	end.

%% @doc セッションオペレーション情報を作成し、ジョブを操作する。
-spec operate(job_operation_args(), job_operation(), job_end_value()) -> 
		  {ok, operation_id()} | {error, term()}.
operate({SessionID}, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, Operation, EndValue}]),
	case kesto_job_session_info:get(SessionID) of
		{ok, _Session} ->
			OperationID = kesto_job_session_operation:make_operation_id(),
			SessionOperation = #job_session_operation{id=OperationID,
													  session_id=SessionID, 
													  operation=Operation, 
													  state=wait,
													  end_value=EndValue,
													  create_timestamp=calendar:local_time()},
			case kesto_job_session_operation:put(SessionOperation) of
				ok -> {ok, OperationID};
				{error, _Error} -> {error, _Error}
			end;
		{error, _Error} -> {error, _Error}
	end;
operate({SessionID, GroupID, JobID}, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, Operation, EndValue}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, _SessionJob} ->
			OperationID = kesto_job_session_operation:make_operation_id(),
			SessionOperation = #job_session_operation{id=OperationID,
													  session_id=SessionID, 
													  job_id=JobID, 
													  group_id=GroupID, 
													  operation=Operation, 
													  state=wait,
													  end_value=EndValue,
													  create_timestamp=calendar:local_time()},
			case kesto_job_session_operation:put(SessionOperation) of
				ok -> {ok, OperationID};
				{error, _Error} -> {error, _Error}
			end;
		{error, _Error} -> {error, _Error}
	end;
operate({SessionID, GroupID, JobID, FacilityID}, Operation, EndValue) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID, Operation, EndValue}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID, FacilityID) of
		{ok, _SessionNode} ->
			OperationID = kesto_job_session_operation:make_operation_id(),
			SessionOperation = #job_session_operation{id=OperationID,
													  session_id=SessionID, 
													  job_id=JobID, 
													  group_id=GroupID, 
													  facility_id=FacilityID,
													  operation=Operation, 
													  state=wait,
													  end_value=EndValue,
													  create_timestamp=calendar:local_time()},
			case kesto_job_session_operation:put(SessionOperation) of
				ok -> {ok, OperationID};
				{error, _Error} -> {error, _Error}
			end;
		{error, _Error} -> {error, _Error}
	end.

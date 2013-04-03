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
%% @doc セッションジョブ(ジョブ)管理モジュール

-module(kesto_job_session_job).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

%% States
-export([
		 init/2,
		 start/1,
		 check/1,
		 reserve/1, 
		 cancel_reserved/1,
		 skip/1,
		 cancel_skip/1,
		 suspend/1,
		 cancel_suspended/1,
		 stop/1,
		 change/1,
		 restart/1,
		 force_stop/1,
		 'end'/1,
		 next/1,
		 terminate/1
		]).

%% @doc ステートを初期化する。
-spec init({session_id(), group_id(), job_id()}, job_operation()) -> {ok, job_session_state()} | {error, term()}.
init({SessionID, GroupID, JobID}, Operation) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, Operation}]),
	Retry = case Operation of
				start -> infinity;
				_ -> ?RETRY_SESSION_LOCK_MAX
			end,
	global:register_name(node(), self()),
	case global:set_lock({{SessionID, GroupID, JobID}, self()}, [node()|nodes()], Retry) of
		true ->
			case kesto_job_session_info:get(SessionID, GroupID, JobID) of
				{ok, SessionJob} ->
					State = #job_session_state{session_info=SessionJob},
					{ok, State};
				{error, _Error} ->
					lager:error("セッションジョブ情報の取得に失敗したため、FSMを終了します。: ~p", [_Error]),
					{error, notfound}
			end;
		false ->
			lager:error("セッションID、グループID、ジョブIDによるロックの取得に失敗したため、FSMを終了します。: ~p", [{SessionID, GroupID, JobID}]),
			{error, lock_error}
	end.

%% @doc セッションジョブを開始する。
-spec start(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
start(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	case kesto_job_session_util:check_previous_end_state(SessionJob) of
		true ->
			case kesto_job_session_util:check_control_condition(SessionJob) of
				true ->
					NewSessionJob = SessionJob#job_session_job{state=running,
															   start_timestamp=calendar:local_time()},
					case kesto_job_session_info:update(NewSessionJob) of
						ok ->
							lager:info("ジョブ[~p]を実行中にしました。", 
									   [{SessionJob#job_session_job.id, 
										 SessionJob#job_session_job.group_id, 
										 SessionJob#job_session_job.job_id}]),
							start_child(State#job_session_state{session_info=NewSessionJob});
						{error, _Error} -> {error, _Error}
					end;
				{false, NewSessionJob} ->
					case kesto_job_session_info:update(NewSessionJob) of
						ok ->
							case NewSessionJob#job_session_job.state of
								reserved ->
									{ok, terminate, State#job_session_state{session_info=NewSessionJob}};
								skip ->
									{ok, 'end', State#job_session_state{session_info=NewSessionJob}}
							end;
						{error, _Error} -> {error, _Error}
					end
			end;
		false ->
			NewSessionJob = SessionJob#job_session_job{state=unexecuted},
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					case kesto_job_session_util:set_state_next_session_job(NewSessionJob, unexecuted) of
						ok ->
							{ok, terminate, State#job_session_state{session_info=NewSessionJob}};
						{error, _Error} -> {error, _Error}
					end;
				{error, _Error} -> {error, _Error}
			end
	end.

%% @doc 直下のセッションノードを開始する。
-spec start_child(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
start_child(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	Command = Conf#job_conf.command,
	FacilityID = Command#job_conf_command.cmd_facility_id,
	case kesto_core_repository:get_node_facility_id_list(FacilityID, true) of
		{ok, FacilityIDList} -> 
			lager:debug("~p", [FacilityIDList]),
			case start_session_node(SessionJob#job_session_job.id, 
									SessionJob#job_session_job.group_id, 
									SessionJob#job_session_job.job_id, 
									FacilityIDList) of
				ok ->
					{ok, check, State};
				{error, _Error} ->
					lager:error("セッションノードの開始に失敗しました。: ~p", 
								[{SessionJob#job_session_job.id, 
								  SessionJob#job_session_job.group_id, 
								  SessionJob#job_session_job.job_id, 
								  _Error}]),
					{error, _Error}
			end;
		{error, _Error} -> 
			lager:error("ノード情報の取得に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id, 
						  FacilityID,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションノードを開始する。
-spec start_session_node(session_id(), group_id(), job_id(), [string()]) -> ok | {error, term()}. 
start_session_node(_SessionID, _GroupID, _JobID, []) ->
	ok;
start_session_node(SessionID, GroupID, JobID, [FacilityID | T]) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID, FacilityID) of
		{ok, _SessionNode} ->
			Preflist = kesto_job_session_util:get_preflist(),
			kesto_job_vnode:operate_session(Preflist, 
											SessionID, 
											GroupID, 
											JobID, 
											FacilityID,
											undefined, start, 0),
			start_session_node(SessionID, GroupID, JobID, T);
		{error, _Error} ->
			lager:error("セッションノードの開始に失敗しました。: ~p", 
						[{SessionID, 
						  GroupID, 
						  JobID, 
						  FacilityID,
						  _Error}]),
			{error, _Error}
	end.

%% @doc 直下のセッションノードのステートを確認する。
-spec check(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
check(State=#job_session_state{session_info=SessionJob}) when ?IS_RUNNING_KESTO_JOB(SessionJob#job_session_job.state) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	Command = Conf#job_conf.command,
	FacilityID = Command#job_conf_command.cmd_facility_id,
	case kesto_core_repository:get_node_facility_id_list(FacilityID, true) of
		{ok, FacilityIDList} -> 
			lager:debug("~p", [FacilityIDList]),
			case get_node_state(SessionJob, 
								FacilityIDList, []) of
				{ok, EndValueList} ->
					lager:debug("~p", [EndValueList]),
					NewSessionJob = case SessionJob#job_session_job.state of
										running ->
											SessionJob#job_session_job{child_end_value=EndValueList};
										stopping ->
											SessionJob#job_session_job{state=stoped,
																	   child_end_value=EndValueList,
																	   end_timestamp=calendar:local_time()}
									end,
					case kesto_job_session_info:update(NewSessionJob) of
						ok ->
							NewState = State#job_session_state{session_info=NewSessionJob},
							case NewSessionJob#job_session_job.state of
								running ->
									{ok, 'end', NewState};
								stoped ->
									{ok, terminate, NewState}
							end;
						{error, _Error} ->
							lager:error("セッションジョブの更新に失敗しました。: ~p", 
										[{NewSessionJob#job_session.id, 
										  NewSessionJob#job_session.group_id, 
										  NewSessionJob#job_session.job_id,
										  _Error}]),
							{error, _Error}
					end;
				{error, running} ->
					lager:debug("~p", [running]),
					{ok, terminate, State};
				{error, _Error} ->
					lager:error("直下のセッションノードの状態確認に失敗しました。: ~p", 
								[{SessionJob#job_session_job.id, 
								  SessionJob#job_session_job.group_id, 
								  SessionJob#job_session_job.job_id, 
								  _Error}]),
					{error, _Error}
			end;
		{error, _Error} -> 
			lager:error("ノード情報の取得に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id, 
						  FacilityID,
						  _Error}]),
			{error, _Error}
	end;
check(State) ->
	lager:debug("~p", [State]),
	{ok, terminate, State}.

%% @doc  直下のセッションジョブのステートを取得する。
-spec get_node_state(job_session_job(), 
					 [string()],
					 [job_end_value()]) -> 
		  {ok, [job_end_value()]} | {error, term()}. 
get_node_state(_SessionJob, [], EndValueList) ->
	{ok, EndValueList};
get_node_state(SessionJob, [FacilityID | T], EndValueList) when SessionJob#job_session_job.state == running ->
	case kesto_job_session_info:get(SessionJob#job_session_job.id, 
									SessionJob#job_session_job.group_id, 
									SessionJob#job_session_job.job_id, 
									FacilityID) of
		{ok, SessionNode} ->
			case ?IS_END_KESTO_JOB(SessionNode#job_session_node.state) of
				true ->
					get_node_state(SessionJob, T, 
								   [SessionNode#job_session_node.end_value | EndValueList]);
				false ->
					{error, running}
			end;
		{error, _Error} -> {error, _Error}
	end;
get_node_state(SessionJob, [FacilityID | T], EndValueList) when SessionJob#job_session_job.state == stopping ->
	case kesto_job_session_info:get(SessionJob#job_session_job.id, 
									SessionJob#job_session_job.group_id, 
									SessionJob#job_session_job.job_id, 
									FacilityID) of
		{ok, SessionNode} ->
			case SessionNode#job_session_node.state == stoped of
				true ->
					get_node_state(SessionJob, T, 
								   [SessionNode#job_session_node.end_value | EndValueList]);
				false ->
					case ?IS_RUNNING_KESTO_JOB(SessionNode#job_session_node.state) of
						true ->
							{error, running};
						false ->
							get_node_state(SessionJob, T, EndValueList)
					end
			end;
		{error, _Error} -> {error, _Error}
	end;
get_node_state(SessionJob, [FacilityID | T], EndValueList) ->
	{error, running}.

%% @doc セッションジョブを中断にする。
-spec suspend(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
suspend(State=#job_session_state{session_info=SessionJob,
								 operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == running of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=suspended},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, terminate, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブの中断を解除する。
-spec cancel_suspended(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
cancel_suspended(State=#job_session_state{session_info=SessionJob,
										  operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == suspended of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=running},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, check, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブを保留にする。
-spec reserve(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
reserve(State=#job_session_state{session_info=SessionJob,
								 operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == wait of
		true ->
			NewConf = kesto_job_session_util:set_control_reserve(
						SessionJob#job_session_job.conf, true),
			NewSessionJob = SessionJob#job_session_job{state=reserved,
													   conf=NewConf},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, terminate, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブの保留を解除する。
-spec cancel_reserved(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
cancel_reserved(State=#job_session_state{session_info=SessionJob,
										 operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == reserved of
		true ->
			NewConf = kesto_job_session_util:set_control_reserve(
						SessionJob#job_session_job.conf, false),
			NewSessionJob = SessionJob#job_session_job{state=wait,
													   conf=NewConf},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, start, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブをスキップにする。
-spec skip(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
skip(State=#job_session_state{session_info=SessionJob,
							  operation_id=OperationID,
							  operation_end_value=EndState}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == wait of
		true ->
			NewConf = kesto_job_session_util:set_control_skip(
						SessionJob#job_session_job.conf, true),
			NewSessionJob = SessionJob#job_session_job{state=skip,
													   end_state=EndState,
													   conf=NewConf},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, terminate, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブのスキップを解除する。
-spec cancel_skip(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
cancel_skip(State=#job_session_state{session_info=SessionJob,
									 operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == skip of
		true ->
			NewConf = kesto_job_session_util:set_control_skip(
						SessionJob#job_session_job.conf, false),
			NewSessionJob = SessionJob#job_session_job{state=wait,
													   conf=NewConf},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, terminate, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブを再実行する。
-spec restart(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
restart(State=#job_session_state{session_info=SessionJob,
								 operation_id=OperationID,
								 operation_end_value=EndState}) ->
	lager:debug("~p", [State]),
	case (SessionJob#job_session_job.state == stopped)
			 or (SessionJob#job_session_job.state == 'end')
			 or (SessionJob#job_session_job.state == changed)
			 or (SessionJob#job_session_job.state == suspended)
			 or (SessionJob#job_session_job.state == error) of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=wait,
													   end_state=undefined,
													   end_value=undefined,
													   start_timestamp=undefined,
													   end_timestamp=undefined},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					case kesto_job_session_util:set_state_next_session_job(NewSessionJob, wait) of
						ok ->
							case kesto_job_session_util:set_state_parent_session_job(NewSessionJob, running) of
								ok ->
									{ok, start, State#job_session_state{session_info=NewSessionJob}};
								{error, _Error} -> {error, _Error}
							end;
						{error, _Error} -> {error, _Error}
					end;
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブの終了値を変更する。
-spec force_stop(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
force_stop(State=#job_session_state{session_info=SessionJob,
									operation_id=OperationID,
									operation_end_value=EndState}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == stopping of
		true ->
			NewSessionJob = SessionJob#job_session_job{state='end',
													   end_state=EndState,
													   end_timestamp=calendar:local_time()},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, next, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブの終了値を変更する。
-spec change(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
change(State=#job_session_state{session_info=SessionJob,
								operation_id=OperationID,
								operation_end_value=EndValue}) ->
	lager:debug("~p", [State]),
	case (SessionJob#job_session_job.state == stoped)
			 or (SessionJob#job_session_job.state == error) of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=changed,
													   end_state=EndValue,
													   end_timestamp=calendar:local_time()},
			NewState = State#job_session_state{session_info=NewSessionJob},
			case kesto_job_session_info:update(NewSessionJob) of
				ok -> 
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, next, NewState};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションジョブを停止する。
-spec stop(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
stop(State=#job_session_state{session_info=SessionJob,
							  operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionJob#job_session_job.state == running of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=stopping,
													   start_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					stop_child(State#job_session_state{session_info=NewSessionJob});
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc 直下のセッションノードを停止する。
-spec stop_child(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
stop_child(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	Command = Conf#job_conf.command,
	FacilityID = Command#job_conf_command.cmd_facility_id,
	case kesto_core_repository:get_node_facility_id_list(FacilityID, true) of
		{ok, FacilityIDList} -> 
			lager:debug("~p", [FacilityIDList]),
			case stop_session_node(SessionJob#job_session_job.id, 
								   SessionJob#job_session_job.group_id, 
								   SessionJob#job_session_job.job_id, 
								   FacilityIDList) of
				ok ->
					{ok, check, State};
				{error, _Error} ->
					lager:error("セッションノードの停止に失敗しました。: ~p", 
								[{SessionJob#job_session_job.id, 
								  SessionJob#job_session_job.group_id, 
								  SessionJob#job_session_job.job_id, 
								  _Error}]),
					{error, _Error}
			end;
		{error, _Error} -> 
			lager:error("ノード情報の取得に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id, 
						  FacilityID,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションノードを停止する。
-spec stop_session_node(session_id(), group_id(), job_id(), [string()]) -> ok | {error, term()}. 
stop_session_node(_SessionID, _GroupID, _JobID, []) ->
	ok;
stop_session_node(SessionID, GroupID, JobID, [FacilityID | T]) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID, FacilityID) of
		{ok, SessionNode} ->
			Preflist = kesto_job_session_util:get_preflist(),
			kesto_job_vnode:operate_session(Preflist, 
											SessionID, 
											GroupID, 
											JobID, 
											FacilityID,
											undefined, stop, 0),
			stop_session_node(SessionID, GroupID, JobID, T);
		{error, _Error} ->
			lager:error("セッションノードの停止に失敗しました。: ~p", 
						[{SessionID, 
						  GroupID, 
						  JobID, 
						  FacilityID,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブを終了する。
-spec 'end'(job_session_state()) -> {ok, job_operate_method(), job_end_state()} | {error, term()}.
'end'(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	EndValueList = SessionJob#job_session_job.child_end_value,
	case get_end_state(SessionJob, EndValueList, info) of
		{ok, EndState} ->
			NewSessionJob = SessionJob#job_session_job{state='end',
													   end_state=EndState,
													   end_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					lager:info("ジョブ[~p]を終了しました。", 
							   [{SessionJob#job_session_job.id, 
								 SessionJob#job_session_job.group_id, 
								 SessionJob#job_session_job.job_id}]),
					NewState = State#job_session_state{session_info=NewSessionJob},
					{ok, next, NewState};
				{error, _Error} ->
					lager:error("セッションジョブの更新に失敗しました。: ~p", 
								[{SessionJob#job_session.id, 
								  SessionJob#job_session.group_id, 
								  SessionJob#job_session.job_id,
								  _Error}]),
					{error, _Error}
			end;
		{error, _Error} ->
			lager:error("セッションジョブの終了状態の取得に失敗しました。: ~p", 
						[{SessionJob#job_session.id, 
						  SessionJob#job_session.group_id, 
						  SessionJob#job_session.job_id,
						  _Error}]),
			{error, _Error}
	end.

-spec get_end_state(job_session_job(), 
					[job_end_state() | job_end_value()], 
					job_end_state()) -> 
		  {ok, job_end_state()} | {error, term()}.
get_end_state(_SessionJob, [], EndState) ->
	{ok, EndState};
get_end_state(SessionJob, [StateOrValue | T], EndState) ->
	Conf = SessionJob#job_session_job.conf,
	TmpEndState = case check_range(StateOrValue, 
								   Conf#job_conf.info_end_value_from, 
								   Conf#job_conf.info_end_value_to) of
					  true -> info;
					  false ->
						  case check_range(StateOrValue, 
										   Conf#job_conf.warning_end_value_from, 
										   Conf#job_conf.warning_end_value_to) of
							  true -> warning;
							  false -> error
						  end
				  end,
	case TmpEndState of
		error -> get_end_state(SessionJob, T, TmpEndState);
		warning ->
			case EndState of
				info -> get_end_state(SessionJob, T, TmpEndState);
				_ -> get_end_state(SessionJob, T, EndState)
			end;
		info -> get_end_state(SessionJob, T, EndState)
	end.

-spec check_range(job_end_value(), integer(), integer()) -> true | false.
check_range(Value, From, To) when (From =< Value) and (Value =< To) -> true;
check_range(_Value, _From, _To) -> false.

%% @doc 後続のセッションジョブを開始する。
-spec next(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
next(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.next_id]),
	case erlang:length(Conf#job_conf.next_id) of
		0 ->
			%% 			case Conf#job_conf.parent_id /= "root" of
			%% 				true ->
			%% 					Preflist = kesto_job_session_util:get_preflist(),
			%% 					kesto_job_vnode:operate_session(Preflist, 
			%% 													SessionJob#job_session_job.id, 
			%% 													SessionJob#job_session_job.group_id, 
			%% 													Conf#job_conf.parent_id,
			%% 													undefined, check, 0),
			%% 					{ok, terminate, State};
			%% 				false ->
			%% 					Preflist = kesto_job_session_util:get_preflist(),
			%% 					kesto_job_vnode:operate_session(Preflist, 
			%% 													SessionJob#job_session_job.id, 
			%% 													undefined, check, 0),
			%% 					{ok, terminate, State}
			%% 			end;
			{ok, terminate, State};
		_ ->
			case start_next_session_job(SessionJob,
										Conf#job_conf.next_id) of
				ok ->
					{ok, terminate, State};
				{error, _Error} ->
					lager:error("エラーが発生したため、FSMを停止します。: ~p", [_Error]),
					{error, _Error}
			end
	end.

%% @doc 後続のセッションジョブを開始する。
-spec start_next_session_job(job_session_job(), [{group_id(), job_id()}]) -> ok | {error, term()}. 
start_next_session_job(_SessionJob, []) ->
	ok;
start_next_session_job(SessionJob, [{GroupID, JobID} | T]) ->
	SessionID = SessionJob#job_session_job.id,
	lager:debug("start : ~p", [{SessionID, GroupID, JobID}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, NextSessionJob} ->
			case kesto_job_session_util:check_start_condition(NextSessionJob) of 
				true ->
					lager:debug("start : ~p", [{SessionID, GroupID, JobID}]),
					Preflist = kesto_job_session_util:get_preflist(),
					kesto_job_vnode:operate_session(Preflist, 
													SessionID, 
													GroupID, 
													JobID,
													undefined, start, 0),
					start_next_session_job(SessionJob, T);
				false ->
					start_next_session_job(SessionJob, T);
				{error, _Error} -> {error, _Error}
			end;
		{error, _Error} -> {error, _Error}
	end.

%% @doc 終了の準備をする。
-spec terminate(job_session_state()) -> ok.
terminate(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	global:del_lock({{SessionJob#job_session_job.id, 
					  SessionJob#job_session_job.group_id, 
					  SessionJob#job_session_job.job_id}, 
					 self()}),
	global:unregister_name(node()),
	ok.

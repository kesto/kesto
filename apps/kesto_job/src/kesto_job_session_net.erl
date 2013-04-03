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
%% @doc セッションジョブ(ネット)管理モジュール

-module(kesto_job_session_net).

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
							lager:info("ジョブネット[~p]を実行中にしました。", 
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

%% @doc 直下のセッションジョブを開始する。
-spec start_child(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
start_child(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.child_id]),
	case start_session_job(SessionJob#job_session_job.id, 
						   Conf#job_conf.child_id) of
		ok ->
			{ok, check, State};
		{error, _Error} ->
			lager:error("直下のセッションジョブの開始に失敗しました。: ~p", 
						[{SessionJob#job_session.id, 
						  SessionJob#job_session.group_id, 
						  SessionJob#job_session.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブを開始する。
-spec start_session_job(job_session_job(), [{group_id(), job_id()}]) -> ok | {error, term()}. 
start_session_job(_SessionID, []) ->
	ok;
start_session_job(SessionID, [{GroupID, JobID} | T]) ->
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			Conf = SessionJob#job_session_job.conf,
			case erlang:length(Conf#job_conf.previous_id) of
				0 ->
					lager:debug("~p", [{SessionID, GroupID, JobID}]),
					Preflist = kesto_job_session_util:get_preflist(),
					kesto_job_vnode:operate_session(Preflist, 
													SessionID, 
													GroupID, 
													JobID,
													undefined, start, 0);
				_ ->
					ok
			end,
			start_session_job(SessionID, T);
		{error, _Error} ->
			lager:error("セッションジョブの開始に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブ情報を確認し、セッションジョブの制御を行う。
-spec check(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
check(State=#job_session_state{session_info=SessionJob}) when ?IS_RUNNING_KESTO_JOB(SessionJob#job_session_job.state) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.child_id]),
	check_child_state(SessionJob#job_session_job.id, 
					  Conf#job_conf.child_id),
	case get_child_state(SessionJob, 
						 Conf#job_conf.child_id, []) of
		{ok, EndStateList} ->
			lager:debug("~p", [EndStateList]),
			NewSessionJob = case SessionJob#job_session_job.state of
								running ->
									SessionJob#job_session_job{child_end_state=EndStateList};
								stopping ->
									SessionJob#job_session_job{state=stoped,
															   child_end_state=EndStateList,
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
			lager:error("直下のセッションジョブの状態確認に失敗しました。: ~p", 
						[{SessionJob#job_session.id, 
						  SessionJob#job_session.group_id, 
						  SessionJob#job_session.job_id,
						  _Error}]),
			{error, _Error}
	end;
check(State) ->
	lager:debug("~p", [State]),
	{ok, terminate, State}.

%% @doc  直下のセッションジョブのステートを取得する。
-spec get_child_state(job_session_job(), 
					  [{group_id(), job_id()}], 
					  [job_end_state()]) -> 
		  {ok, [job_end_state()]} | {error, term()}. 
get_child_state(_SessionJob, [], EndStateList) ->
	{ok, EndStateList};
get_child_state(SessionJob, [{GroupID, JobID} | T], EndStateList) when SessionJob#job_session_job.state == running ->
	SessionID = SessionJob#job_session_job.id,
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, Child} ->
			State = Child#job_session_job.state,
			case ?IS_END_KESTO_JOB(State) of
				true ->
					Conf = Child#job_session_job.conf,
					case erlang:length(Conf#job_conf.next_id) of
						0 ->
							case Conf#job_conf.recovery of
								true ->
									case State == unexecuted of
										true ->
											get_child_state(SessionJob, T, EndStateList);
										false ->
											get_child_state(SessionJob, T, 
															[error | EndStateList])
									end;
								false ->
									case State == unexecuted of
										true ->
											get_child_state(SessionJob, T, EndStateList);
										false ->
											get_child_state(SessionJob, T, 
															[Child#job_session_job.end_state | EndStateList])
									end
							end;
						_ ->
							get_child_state(SessionJob, T, EndStateList)
					end;
				false ->
					{error, running}
			end;
		{error, _Error} ->
			lager:error("セッションジョブの取得に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end;
get_child_state(SessionJob, [{GroupID, JobID} | T], EndStateList) when SessionJob#job_session_job.state == stopping ->
	SessionID = SessionJob#job_session_job.id,
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, Child} ->
			State = Child#job_session_job.state,
			case State == stoped of
				true ->
					Conf = Child#job_session_job.conf,
					case erlang:length(Conf#job_conf.next_id) of
						0 ->
							case Conf#job_conf.recovery of
								true ->
									case State == unexecuted of
										true ->
											get_child_state(SessionJob, T, EndStateList);
										false ->
											get_child_state(SessionJob, T, 
															[error | EndStateList])
									end;
								false ->
									get_child_state(SessionJob, T, 
													[Child#job_session_job.end_state | EndStateList])
							end;
						_ ->
							get_child_state(SessionJob, T, EndStateList)
					end;
				false ->
					case ?IS_RUNNING_KESTO_JOB(State) of
						true ->
							{error, running};
						false ->
							get_child_state(SessionJob, T, EndStateList)
					end
			end;
		{error, _Error} ->
			lager:error("セッションジョブの取得に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end;
get_child_state(SessionJob, [{GroupID, JobID} | T], EndStateList) ->
	{error, running}.

%% @doc  直下のセッションジョブのステートを取得する。
-spec check_child_state(session_id(), [{group_id(), job_id()}]) -> ok | {error, term()}. 
check_child_state(_SessionID, []) ->
	ok;
check_child_state(SessionID, [{GroupID, JobID} | T]) ->
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			case (SessionJob#job_session_job.state == running) 
					 or (SessionJob#job_session_job.state == stopping) of
				true ->
					lager:debug("start : ~p", [{SessionID, GroupID, JobID}]),
					Preflist = kesto_job_session_util:get_preflist(),
					kesto_job_vnode:operate_session(Preflist, 
													SessionID, 
													GroupID, 
													JobID,
													undefined, check, 0),
					check_child_state(SessionID, T);
				false ->
					check_child_state(SessionID, T)
			end;
		{error, _Error} ->
			lager:error("セッションジョブの取得に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

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

%% @doc 直下のセッションジョブを停止する。
-spec stop_child(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
stop_child(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.child_id]),
	case stop_session_job(SessionJob#job_session_job.id, 
						  Conf#job_conf.child_id) of
		ok ->
			{ok, check, State};
		{error, _Error} ->
			lager:error("直下のセッションジョブの停止に失敗しました。: ~p", 
						[{SessionJob#job_session.id, 
						  SessionJob#job_session.group_id, 
						  SessionJob#job_session.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブを停止する。
-spec stop_session_job(job_session_job(), [{group_id(), job_id()}]) -> ok | {error, term()}. 
stop_session_job(_SessionID, []) ->
	ok;
stop_session_job(SessionID, [{GroupID, JobID} | T]) ->
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			Preflist = kesto_job_session_util:get_preflist(),
			kesto_job_vnode:operate_session(Preflist, 
											SessionID, 
											GroupID, 
											JobID,
											undefined, stop, 0),
			stop_session_job(SessionID, T);
		{error, _Error} ->
			lager:error("セッションジョブの停止に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブを終了する。
-spec 'end'(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
'end'(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	EndStateList = SessionJob#job_session_job.child_end_state,
	case get_end_state(SessionJob, EndStateList, info) of
		{ok, EndState} ->
			NewSessionJob = SessionJob#job_session_job{state='end',
													   end_state=EndState,
													   end_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					lager:info("ジョブネット[~p]を終了しました。", 
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

-spec get_end_state(job_session_job(), [job_end_state()], job_end_state()) -> 
		  {ok, job_end_state()} | {error, term()}.
get_end_state(_SessionJob, [], EndState) ->
	{ok, EndState};
get_end_state(SessionJob, [State | T], EndState) ->
	case State of
		error -> get_end_state(SessionJob, T, State);
		warning ->
			case EndState of
				info -> get_end_state(SessionJob, T, State);
				_ -> get_end_state(SessionJob, T, EndState)
			end;
		info -> get_end_state(SessionJob, T, EndState);
		_ -> get_end_state(SessionJob, T, EndState)
	end.

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

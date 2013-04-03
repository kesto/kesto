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
%% @doc セッションジョブ(判定ジョブ)管理モジュール

-module(kesto_job_session_judgment).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

%% States
-export([
		 init/2,
		 start/1,
		 check/1, 
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
			Conf = SessionJob#job_session_job.conf,
			case get_previous_state(SessionJob#job_session_job.id, 
									Conf#job_conf.previous_id) of
				{ok, EndState} ->
					lager:debug("~p", [EndState]),
					NewSessionJob = SessionJob#job_session_job{state=running,
															   previous_end_state=EndState,
															   start_timestamp=calendar:local_time()},
					case kesto_job_session_info:update(NewSessionJob) of
						ok ->
							lager:info("判定ジョブ[~p]を実行中にしました。", 
									   [{SessionJob#job_session_job.id, 
										 SessionJob#job_session_job.group_id, 
										 SessionJob#job_session_job.job_id}]),
							start_dependent(State#job_session_state{session_info=NewSessionJob});
						{error, _Error} -> {error, _Error}
					end;
				{error, _Error} -> {error, _Error}
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

%% @doc  先行セッションジョブの終了状態を取得する。
-spec get_previous_state(session_id(), [{group_id(), job_id()}]) -> 
		  {ok, job_end_state()} | {error, term()}. 
get_previous_state(SessionID, [{GroupID, JobID}]) ->
	lager:debug("~p", [{SessionID, GroupID, JobID}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			Conf = SessionJob#job_session_job.conf,
			case Conf#job_conf.type of
				judgment ->
					PreviousID = Conf#job_conf.previous_id,
					case erlang:length(PreviousID) of
						0 -> {ok, info};
						1 -> get_previous_state(SessionID, PreviousID);
						_ -> {ok, info}
					end;
				_ -> {ok, SessionJob#job_session_job.end_state}
			end;
		{error, _Error} ->
			{error, _Error}
	end.

%% @doc 従属セッションジョブを開始する。
-spec start_dependent(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
start_dependent(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	case start_session_job(SessionJob,
						   Conf#job_conf.dependent_id) of
		ok ->
			lager:info("判定ジョブ[~p]の従属ジョブ[~p]を実行します。", 
					   [{SessionJob#job_session_job.id, 
						 SessionJob#job_session_job.group_id, 
						 SessionJob#job_session_job.job_id},
						Conf#job_conf.dependent_id]),
			{ok, check, State};
		{error, _Error} ->
			lager:error("従属セッションジョブの開始に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc 従属セッションジョブを開始する。
-spec start_session_job(job_session_job(), {group_id(), job_id()}) -> ok | {error, term()}. 
start_session_job(SessionJob, {GroupID, JobID}) ->
	SessionID = SessionJob#job_session_job.id,
	Conf = SessionJob#job_session_job.conf,
	Judgement = Conf#job_conf.judgement,
	Value = Judgement#job_conf_judgement.value,
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, TargetSessionJob} ->
			TargetConf = TargetSessionJob#job_session_job.conf,
			Start = case Judgement#job_conf_judgement.type of
						end_state ->
							case Value == SessionJob#job_session_job.previous_end_state of
								true ->
									case TargetConf#job_conf.recovery of
										true -> false;
										false -> true
									end;
								false -> false
							end;
						parameter ->
							false
					end,
			lager:debug("start : ~p", [{SessionID, GroupID, JobID, TargetConf#job_conf.recovery, Start}]),
			case Start of
				true ->
					lager:debug("start : ~p", [{SessionID, GroupID, JobID}]),
					Preflist = kesto_job_session_util:get_preflist(),
					kesto_job_vnode:operate_session(Preflist, 
													SessionID, 
													GroupID, 
													JobID,
													undefined, start, 0),
					ok;
				false ->
					lager:debug("not start : ~p", [{SessionID, GroupID, JobID}]),
					NewTargetSessionJob = TargetSessionJob#job_session_job{state=unexecuted},
					case kesto_job_session_info:update(NewTargetSessionJob) of
						ok ->
							kesto_job_session_util:set_state_next_session_job(NewTargetSessionJob, unexecuted);
						{error, _Error} -> {error, _Error}
					end
			end;
		{error, _Error} ->
			lager:error("セッションジョブの開始に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブ情報を確認し、セッションジョブの制御を行う。
-spec check(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
check(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.dependent_id]),
	check_dependent_state(SessionJob#job_session_job.id, 
						  Conf#job_conf.dependent_id),
	case get_dependent_state(SessionJob#job_session_job.id, 
							 Conf#job_conf.dependent_id) of
		{ok, EndState} ->
			lager:debug("~p", [EndState]),
			NewSessionJob = SessionJob#job_session_job{dependent_end_state=EndState},
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					NewState = State#job_session_state{session_info=NewSessionJob},
					{ok, 'end', NewState};
				{error, _Error} ->
					lager:error("セッションジョブの更新に失敗しました。: ~p", 
								[{NewSessionJob#job_session_job.id, 
								  NewSessionJob#job_session_job.group_id, 
								  NewSessionJob#job_session_job.job_id,
								  _Error}]),
					{error, _Error}
			end;
		{error, running} ->
			lager:debug("~p", [running]),
			{ok, terminate, State};
		{error, _Error} ->
			lager:error("直下のセッションジョブの状態確認に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc  直下のセッションジョブのステートを取得する。
-spec get_dependent_state(session_id(), {group_id(), job_id()}) -> 
		  {ok, job_end_state()} | {error, term()}. 
get_dependent_state(SessionID, {GroupID, JobID}) ->
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			State = SessionJob#job_session_job.state,
			case ?IS_END_KESTO_JOB(State) of
				true ->
					{ok, SessionJob#job_session_job.end_state};
				false ->
					{error, running}
			end;
		{error, _Error} ->
			lager:error("従属セッションジョブの取得に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

%% @doc 従属セッションジョブのステートを取得する。
-spec check_dependent_state(session_id(), {group_id(), job_id()}) -> ok | {error, term()}. 
check_dependent_state(SessionID, {GroupID, JobID}) ->
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			case SessionJob#job_session_job.state == running of
				true ->
					lager:debug("start : ~p", [{SessionID, GroupID, JobID}]),
					Preflist = kesto_job_session_util:get_preflist(),
					kesto_job_vnode:operate_session(Preflist, 
													SessionID, 
													GroupID, 
													JobID,
													undefined, check, 0),
					ok;
				false ->
					ok
			end;
		{error, _Error} ->
			lager:error("セッションジョブの取得に失敗しました。: ~p", 
						[{SessionID, GroupID, JobID, _Error}]),
			{error, _Error}
	end.

%% @doc セッションジョブを終了する。
-spec 'end'(job_session_state()) -> {ok, job_operate_method(), job_end_state()} | {error, term()}.
'end'(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	NewSessionJob = case SessionJob#job_session_job.dependent_end_state == undefined of
						true ->
							SessionJob#job_session_job{state='end',
													   end_state=SessionJob#job_session_job.previous_end_state,
													   end_timestamp=calendar:local_time()};
						false ->
							SessionJob#job_session_job{state='end',
													   end_state=SessionJob#job_session_job.dependent_end_state,
													   end_timestamp=calendar:local_time()}
					end,
	case kesto_job_session_info:update(NewSessionJob) of
		ok ->
			lager:info("判定ジョブ[~p]を終了しました。", 
					   [{SessionJob#job_session_job.id, 
						 SessionJob#job_session_job.group_id, 
						 SessionJob#job_session_job.job_id}]),
			NewState = State#job_session_state{session_info=NewSessionJob},
			{ok, next, NewState};
		{error, _Error} ->
			lager:error("セッションジョブの更新に失敗しました。: ~p", 
						[{SessionJob#job_session_job.id, 
						  SessionJob#job_session_job.group_id, 
						  SessionJob#job_session_job.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc 後続のセッションジョブを開始する。
-spec next(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
next(State=#job_session_state{session_info=SessionJob}) ->
	lager:debug("~p", [State]),
	Conf = SessionJob#job_session_job.conf,
	lager:debug("~p", [Conf#job_conf.next_id]),
	case start_next_session_job(SessionJob,
								Conf#job_conf.next_id) of
		ok ->
			{ok, terminate, State};
		{error, _Error} ->
			lager:error("エラーが発生したため、FSMを停止します。: ~p", [_Error]),
			{error, _Error}
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

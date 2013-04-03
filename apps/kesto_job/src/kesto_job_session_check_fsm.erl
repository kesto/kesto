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
%% @doc セッションチェック用FSMモジュール

-module(kesto_job_session_check_fsm).
-behaviour(gen_fsm).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

-export([
		 start_link/0, 
		 init/1, 
		 code_change/4, 
		 handle_event/3, 
		 handle_info/3,
		 handle_sync_event/4, 
		 terminate/3
		]).

-export([
		 prepare/2, 
		 waiting/2, 
		 check_next_time/2, 
		 check/2
		]).

-record(state, {next,
				interval,
				offset}).

%% @doc スケジュール定義チェック用gen_fsmを開始。
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%% @doc 初期化処理
-spec init([]) -> {ok, atom(), #state{}, integer()}.
init([]) ->
	process_flag(trap_exit, true),
	Interval = kesto_job_config:check_interval(),
	Offset = kesto_job_config:check_offset(),
	StateData=#state{interval=Interval,
					 offset=Offset},
	{ok, prepare, StateData, 5000}.

prepare(timeout, StateData=#state{interval=Interval,
								  offset=Offset}) ->
	lager:debug("~p", [StateData]),
	Next = get_next_time(Interval, Offset),
	NewStateData = StateData#state{next=Next},
	{next_state, waiting, NewStateData, 0}.

waiting(timeout, StateData) ->
	lager:debug("~p", [StateData]),
	{next_state, check_next_time, StateData, 1000}.

check_next_time(timeout, StateData=#state{interval=Interval,
										  next=Next}) ->
	lager:debug("~p", [StateData]),
	case Next < calendar:local_time() of
		true ->
			NewNext = calendar:gregorian_seconds_to_datetime(
						calendar:datetime_to_gregorian_seconds(Next) + Interval),
			NewStateData = StateData#state{next=NewNext},
			{next_state, check, NewStateData, 0};
		false ->
			{next_state, waiting, StateData, 0}
	end.

check(timeout, StateData) ->
	lager:debug("~p", [StateData]),
	case kesto_job_session_info:get_list(running, job_session) of
		{ok, List} -> 
			check_state(List),
			check_operation(List),
			check_delay(List),
			{next_state, waiting, StateData, 0};
		{error, _Error} -> {next_state, waiting, StateData, 0}
	end.

%% @doc セッションジョブオペレーション情報をチェックする。
-spec check_operation([job_session()]) -> ok.
check_operation([]) ->
	ok;
check_operation([Session | T]) ->
	lager:debug("~p", [Session]),
	case kesto_job_session_operation:get_list(Session#job_session.id, running) of
		{ok, RunningList} ->
			case RunningList of
				[_Running | _] ->
					check_operation(T);
				[] ->
					case kesto_job_session_operation:get_list(Session#job_session.id, wait) of
						{ok, WaitList} ->
							case WaitList of
								[Wait | _] ->
									operate(Wait),
									kesto_job_session_operation:setState(Wait#job_session_operation.id, 
																		 running,
																		 "",
																		 calendar:local_time());
								[] ->
									check_operation(T)
							end;
						{error, _Error} -> {error, _Error}
					end
			end;
		{error, _Error} -> {error, _Error}
	end.

%% @doc セッションジョブオペレーション情報を元に操作を実施する。
-spec operate(job_session_operation()) -> ok.
operate(_OperationInfo = #job_session_operation{id=OperationID,
												session_id=SessionID, 
												job_id=undefined, 
												group_id=undefined, 
												facility_id=undefined, 
												operation=Operation, 
												end_value=EndValue}) ->
	
	kesto_job_vnode:operate_session(kesto_job_session_util:get_preflist(),
									SessionID, 
									OperationID, 
									Operation, 
									EndValue);
operate(_OperationInfo = #job_session_operation{id=OperationID,
												session_id=SessionID, 
												job_id=JobID, 
												group_id=GroupID, 
												facility_id=undefined, 
												operation=Operation, 
												end_value=EndValue}) ->
	
	kesto_job_vnode:operate_session(kesto_job_session_util:get_preflist(),
									SessionID, 
									GroupID,
									JobID,
									OperationID, 
									Operation, 
									EndValue);
operate(_OperationInfo = #job_session_operation{id=OperationID,
												session_id=SessionID, 
												job_id=JobID, 
												group_id=GroupID, 
												facility_id=FacilityID, 
												operation=Operation, 
												end_value=EndValue}) ->
	
	kesto_job_vnode:operate_session(kesto_job_session_util:get_preflist(),
									SessionID, 
									GroupID,
									JobID,
									FacilityID,
									OperationID, 
									Operation, 
									EndValue).

%% @doc セッションジョブの開始遅延をチェックする。
-spec check_state([job_session()]) -> ok.
check_state([]) ->
	ok;
check_state([Session | T]) ->
	lager:debug("~p", [Session]),
	Preflist = kesto_job_session_util:get_preflist(),
	kesto_job_vnode:operate_session(Preflist, 
									Session#job_session.id, 
									undefined, 
									check, 0),
	check_state(T).

%% @doc セッションジョブの開始遅延をチェックする。
-spec check_delay([job_session()]) -> ok.
check_delay([]) ->
	ok;
check_delay([Session | T]) ->
	lager:debug("~p", [Session]),
	case kesto_job_session_info:get_list(Session#job_session.id, wait, job_session_job) of
		{ok, WaitSessionJobList} ->
			check_start_delay(Session#job_session.start_timestamp, 
							  WaitSessionJobList);
		{error, _Error1} ->
			lager:error("待機中のセッションジョブリストの取得に失敗しました。: ~p", [_Error1])
	end,
	case kesto_job_session_info:get_list(Session#job_session.id, running, job_session_job) of
		{ok, RunningSessionJobList} ->
			check_end_delay(Session#job_session.start_timestamp, 
							RunningSessionJobList);
		{error, _Error2} ->
			lager:error("実行中のセッションジョブリストの取得に失敗しました。: ~p", [_Error2])
	end,
	check_delay(T).

%% @doc セッションジョブの開始遅延をチェックする。
-spec check_start_delay(datetime(), [job_session_job()]) -> ok.
check_start_delay(_, []) ->
	ok;
check_start_delay(SessionStartTime, [SessionJob | T]) ->
	lager:debug("~p", [{SessionStartTime, SessionJob}]),
	Conf = SessionJob#job_session_job.conf,
	case is_record(Conf#job_conf.delay, job_conf_delay) of
		true ->
			Delay = Conf#job_conf.delay,
			case Delay#job_conf_delay.start_delay of
				true ->
					Check1 = check_delay_session_job(Delay#job_conf_delay.start_delay_session, 
													 Delay#job_conf_delay.start_delay_session_value, 
													 SessionStartTime),
					Check2 = check_delay_time(Delay#job_conf_delay.start_delay_session, 
											  Delay#job_conf_delay.start_delay_session_value, 
											  SessionStartTime),
					case Check1 or Check2 of
						true ->
							%% 通知＆アクション
							lager:info("遅延してるよー ~p", [{SessionStartTime, SessionJob}]),
							ok;
						false -> check_start_delay(SessionStartTime, T)
					end;
				false -> check_start_delay(SessionStartTime, T)
			end;
		false -> check_start_delay(SessionStartTime, T)
	end.

%% @doc セッションジョブの終了遅延をチェックする。
-spec check_end_delay(datetime(), [job_session_job()]) -> ok.
check_end_delay(_, []) ->
	ok;
check_end_delay(SessionStartTime, [SessionJob | T]) ->
	lager:debug("~p", [{SessionStartTime, SessionJob}]),
	Conf = SessionJob#job_session_job.conf,
	case is_record(Conf#job_conf.delay, job_conf_delay) of
		true ->
			Delay = Conf#job_conf.delay,
			case Delay#job_conf_delay.end_delay of
				true ->
					Check1 = check_delay_session_job(Delay#job_conf_delay.end_delay_session, 
													 Delay#job_conf_delay.end_delay_session_value, 
													 SessionStartTime),
					Check2 = check_delay_session_job(Delay#job_conf_delay.end_delay_job, 
													 Delay#job_conf_delay.end_delay_job_value, 
													 SessionJob#job_session_job.start_timestamp),
					Check3 = check_delay_time(Delay#job_conf_delay.end_delay_session, 
											  Delay#job_conf_delay.end_delay_session_value, 
											  SessionStartTime),
					case Check1 or Check2 or Check3 of
						true ->
							%% 通知＆アクション
							lager:info("遅延してるよー ~p", [{SessionStartTime, SessionJob}]),
							ok;
						false -> check_start_delay(SessionStartTime, T)
					end;
				false -> check_end_delay(SessionStartTime, T)
			end;
		false -> check_end_delay(SessionStartTime, T)
	end.

%% @doc セッションまたはジョブの開始からの経過時間による遅延を確認する。
-spec check_delay_session_job(boolean(), integer(), datetime()) -> true | false.
check_delay_session_job(Enabled, DelayValue, BaseDateTime) ->
	lager:debug("~p", [{Enabled, DelayValue, BaseDateTime}]),
	case Enabled of
		true ->
			Threshold = calendar:gregorian_seconds_to_datetime(
						  calendar:datetime_to_gregorian_seconds(
							BaseDateTime) + DelayValue * 60),
			lager:debug("~p", [Threshold]),
			Threshold < calendar:local_time();
		false -> false
	end.

%% @doc 時刻による遅延を確認する。
-spec check_delay_time(boolean(), time(), datetime()) -> true | false.
check_delay_time(Enabled, DelayValue, BaseDateTime) ->
	lager:debug("~p", [{Enabled, DelayValue, BaseDateTime}]),
	case Enabled of
		true ->
			{{Year, Month, Day}, BaseTime} = BaseDateTime,
			{Hour, Minute, Second} = DelayValue,
			Threshold = case BaseTime < DelayValue of
							true ->
								{{Year, Month, Day}, {Hour, Minute, Second}};
							false ->
								Tmp = {{Year, Month, Day}, {Hour, Minute, Second}},
								calendar:gregorian_seconds_to_datetime(
								  calendar:datetime_to_gregorian_seconds(
									Tmp) + 24 * 60 * 60)
						end,
			(BaseDateTime < Threshold) and (Threshold =< calendar:local_time());
		false -> false
	end.

get_next_time(Interval, Offset) ->
	lager:debug("~p", [{Interval, Offset}]),
	
	Now = calendar:local_time(),
	{_, {_, Minutes, Seconds}} = Now,
	
	Start = Minutes * 60 + Seconds,
	End = Start + Interval * 2,
	Plus = find_start(
			 [X || X <- lists:seq(Start, End), X rem Interval == 0], 
			 Start),
	
	calendar:gregorian_seconds_to_datetime(
	  calendar:datetime_to_gregorian_seconds(
		Now) - Start + Plus + Offset).

find_start([], _Time) ->
	60 * 60;
find_start([Seconds | T], Time) ->
	if
		Seconds > Time -> Seconds;
		true -> find_start(T, Time)
	end.

handle_info(_Info, _StateName, StateData) ->
	{stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
	{stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
	{stop, badmsg, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.
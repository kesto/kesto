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
%% @doc セッションジョブ管理ユーティリティモジュール

-module(kesto_job_session_util).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

-export([
		 get_parent_session_job/1,
		 get_previous_session_job/1,
		 check_start_condition/1,
		 check_control_condition/1,
		 check_previous_end_state/1,
		 is_end_session_job/1,
		 is_first_job/1,
		 is_root_jobnet/1,
		 is_recovery_job/1,
		 is_judgment_job/1,
		 is_end_state/2,
		 is_end_state/3,
		 set_control_reserve/2,
		 set_control_skip/2,
		 get_preflist/0,
		 set_state_next_session_job/2,
		 set_state_parent_session_job/2
		]).

%% @doc セッションジョブ及びセッションノードの上位のセッションジョブを取得する。
-spec get_parent_session_job(job_session_job() | job_session_node()) -> 
		  {ok, job_session_job()} | {error, term()}.
get_parent_session_job(SessionJob) when is_record(SessionJob, job_session_job) ->
	Conf = SessionJob#job_session_job.conf,
	kesto_job_session_info:get(SessionJob#job_session_job.id,
							   SessionJob#job_session_job.group_id,
							   Conf#job_conf.parent_id);
get_parent_session_job(SessionNode) when is_record(SessionNode, job_session_node) ->
	kesto_job_session_info:get(SessionNode#job_session_node.id,
							   SessionNode#job_session_node.group_id,
							   SessionNode#job_session_node.job_id).

%% @doc セッションジョブの先行セッションジョブを取得する。
-spec get_previous_session_job(job_session_job()) -> 
		  {ok, [job_session_job()]} | {error, term()}.
get_previous_session_job(SessionJob) ->
	Conf = SessionJob#job_session_job.conf,
	get_previous_session_job(SessionJob#job_session_job.id,
							 Conf#job_conf.previous_id,
							 []).
-spec get_previous_session_job(session_id(), [{group_id(), job_id()}], [job_session_job()]) -> 
		  {ok, [job_session_job()]} | {error, term()}.
get_previous_session_job(_, [], SessionJobList) ->
	{ok, SessionJobList};
get_previous_session_job(SessionID, [{GroupID, JobID} | T], SessionJobList) ->
	case kesto_job_session_info:get(SessionID,
									GroupID, 
									JobID) of
		{ok, SessionJob} ->
			get_previous_session_job(SessionID, T, [SessionJob | SessionJobList]);
		{error, _Error} -> {error, _Error}
	end.

%% @doc セッションジョブが終了しているか確認する。
-spec is_end_session_job([job_session_job()]) -> true | false.
is_end_session_job([SessionJob | T]) ->
	Conf = SessionJob#job_session_job.conf,
	case ?IS_END_KESTO_JOB(SessionJob#job_session_job.state) of
		true -> is_end_session_job(T);
		false -> false
	end.

%% @doc セッションジョブがジョブネットの最初のジョブか確認する。
-spec is_first_job(job_session_job()) -> true | false.
is_first_job(SessionJob) ->
	Conf = SessionJob#job_session_job.conf,
	case erlang:length(Conf#job_conf.previous_id) of
		0 -> true;
		_ -> false
	end.

%% @doc セッションジョブがルートジョブネットか確認する。
-spec is_root_jobnet(job_session_job()) -> true | false.
is_root_jobnet(SessionJob) ->
	Conf = SessionJob#job_session_job.conf,
	Conf#job_conf.parent_id == "root".

%% @doc セッションジョブが判定ジョブか確認する。
-spec is_judgment_job(job_session_job()) -> true | false.
is_judgment_job(SessionJob) ->
	Conf = SessionJob#job_session_job.conf,
	Conf#job_conf.type == judgment.

%% @doc セッションジョブがリカバリジョブまたはリカバリジョブネットか確認する。
-spec is_recovery_job(job_session_job() | [job_session_job()]) -> true | false.
is_recovery_job(SessionJob) when is_record(SessionJob, job_session_job) ->
	Conf = SessionJob#job_session_job.conf,
	Conf#job_conf.recovery;
is_recovery_job(SessionJobList) when is_list(SessionJobList) ->
	List = lists:map(fun(X) ->
							 X#job_session_job.conf#job_conf.recovery
					 end, 
					 SessionJobList),
	not(lists:member(false, List)).

%% @doc セッションジョブがリカバリジョブまたはリカバリジョブネットか確認する。
-spec is_end_state(job_end_state(), job_session_job()) -> true | false.
is_end_state(EndState, SessionJob) ->
	SessionJob#job_session_job.end_state == EndState.
-spec is_end_state(job_end_state(), [job_session_job()], all | worst) -> true | false.
is_end_state(EndState, SessionJobList, Mode) ->
	case Mode of
		all ->
			List = lists:map(fun(X) -> 
									 X#job_session_job.end_state == EndState 
							 end, 
							 SessionJobList),
			not(lists:member(false, List));
		worst ->
			List = lists:map(fun(X) -> 
									 X#job_session_job.end_state 
							 end, 
							 SessionJobList),
			case lists:member(error, List) of
				true -> EndState == error;
				false ->
					case lists:member(warning, List) of
						true -> EndState == warning;
						false ->
							case lists:member(info, List) of
								true -> EndState == info;
								false -> false
							end
					end
			end
	end.

%% @doc 先行セッションジョブの実行状態による開始可否を確認する。
-spec check_start_condition(job_session_job()) -> true | false | {error, term()}.
check_start_condition(SessionJob) ->
	lager:debug("~p", [SessionJob]),
	case is_first_job(SessionJob) of
		true ->
			% セッションジョブが、ジョブネットの最初の場合
			case is_root_jobnet(SessionJob) of
				true -> true;
				false -> 
					case get_parent_session_job(SessionJob) of
						{ok, ParentJob} ->
							% ジョブネットが実行中ならば、開始可能
							ParentJob#job_session_job.state == running;
						{error, _Error} -> 
							lager:error("セッションジョブの開始条件チェックに失敗した為、開始不可とします。: ~p", 
										[{SessionJob#job_session_job.id, 
										  SessionJob#job_session_job.group_id, 
										  SessionJob#job_session_job.job_id,
										  _Error}]),
							false
					end
			end;
		false ->
			% セッションジョブに先行ジョブが存在する場合
			case get_previous_session_job(SessionJob) of
				{ok, List} ->
					case is_judgment_job(SessionJob) of
						true ->
							% 判定ジョブの場合
							%   開始可能:先行セッションジョブが1つ
							%   開始不可:先行セッションジョブが複数
							erlang:length(List) == 1;
						false -> true
					end;
				{error, _Error} ->
					lager:error("セッションジョブの開始条件チェックに失敗した為、開始不可とします。: ~p", 
								[{SessionJob#job_session_job.id, 
								  SessionJob#job_session_job.group_id, 
								  SessionJob#job_session_job.job_id,
								  _Error}]),
					false
			end
	end.

%% @doc 先行セッションジョブの終了状態による開始可否を確認する。
-spec check_previous_end_state(job_session_job()) -> true | false.
check_previous_end_state(SessionJob) ->
	lager:debug("~p", [SessionJob]),
	case is_first_job(SessionJob) of
		true ->
			% セッションジョブが、ジョブネットの最初の場合
			case is_root_jobnet(SessionJob) of
				true -> true;
				false -> 
					case get_parent_session_job(SessionJob) of
						{ok, ParentJob} ->
							% ジョブネットが実行中ならば、開始可能
							ParentJob#job_session_job.state == running;
						{error, _Error} -> 
							lager:error("セッションジョブの開始条件チェックに失敗した為、開始不可とします。: ~p", 
										[{SessionJob#job_session_job.id, 
										  SessionJob#job_session_job.group_id, 
										  SessionJob#job_session_job.job_id,
										  _Error}]),
							false
					end
			end;
		false ->
			% セッションジョブに先行ジョブが存在する場合
			case get_previous_session_job(SessionJob) of
				{ok, List} ->
					case is_judgment_job(SessionJob) of
						true ->
							lager:debug("~p", [List]),
							% 判定ジョブの場合
							%   開始可能:先行セッションジョブが1つ
							%   開始不可:先行セッションジョブが複数
							erlang:length(List) == 1;
						false -> 
							case is_recovery_job(SessionJob) of
								true -> 
									% リカバリジョブまたはリカバリジョブネットの場合
									%   開始可能:すべての先行ジョブがリカバリジョブまたはリカバリジョブネット
									%   開始不可:すべての先行ジョブがその他
									case is_recovery_job(List) of
										true -> not(is_end_state(error, List, all));
										false -> is_end_state(error, List, all)
									end;
								false -> 
									% その他の場合
									%   開始可能:すべての先行ジョブがその他
									%	開始不可:すべての先行ジョブがリカバリジョブまたはリカバリジョブネット
									case is_recovery_job(List) of
										true -> false;
										false -> not(is_end_state(error, List, worst))
									end
							end
					end;
				{error, _Error} ->
					lager:error("セッションジョブの開始条件チェックに失敗した為、開始不可とします。: ~p", 
								[{SessionJob#job_session_job.id, 
								  SessionJob#job_session_job.group_id, 
								  SessionJob#job_session_job.job_id,
								  _Error}]),
					false
			end
	end.

%% @doc セッションジョブの制御定義によりセッションジョブの実行状態を操作する。
-spec check_control_condition(job_session_job()) -> true | {false, job_session_job()}.
check_control_condition(SessionJob) ->
	lager:debug("~p", [SessionJob]),
	Conf = SessionJob#job_session_job.conf,
	Control = Conf#job_conf.control,
	case Control#job_conf_control.control_reserve of
		true ->
			NewSessionJob = SessionJob#job_session_job{state=reserved},
			{false, NewSessionJob};
		false ->
			case Control#job_conf_control.control_skip of
				true ->
					EndState = Control#job_conf_control.control_end_state,
					NewSessionJob = SessionJob#job_session_job{state=skip,
															   end_state=EndState},
					{false, NewSessionJob};
				false ->
					case SessionJob#job_session_job.state == skip of
						true ->
							{false, SessionJob};
						false ->
							true
					end
			end
	end.

-spec set_control_reserve(job_conf(), true | false) -> job_conf().
set_control_reserve(Conf, Value) ->
	Control = Conf#job_conf.control,
	NewControl = Control#job_conf_control{control_reserve=Value},
	Conf#job_conf{control=NewControl}.

-spec set_control_skip(job_conf(), true | false) -> job_conf().
set_control_skip(Conf, Value) ->
	Control = Conf#job_conf.control,
	NewControl = Control#job_conf_control{control_skip=Value},
	Conf#job_conf{control=NewControl}.

-spec get_preflist() -> term().
get_preflist() ->
	DocIdx = riak_core_util:chash_key({<<"kesto_job_session_util">>,
									   term_to_binary(now())}),
	Preflist = riak_core_apl:get_apl(DocIdx, 1, kesto_job),
	[IdxNode] = Preflist,
	IdxNode.

%% @doc セッションジョブの後続セッションジョブの実行状態を設定する。
-spec set_state_next_session_job(job_session_job(), job_state()) -> 
		  ok | {error, term()}.
set_state_next_session_job(SessionJob, State) ->
	lager:info("~p", [{SessionJob, State}]),
	Conf = SessionJob#job_session_job.conf,
	case Conf#job_conf.type of
		judgment ->
			set_state_next_session_job(SessionJob#job_session_job.id,
									   [Conf#job_conf.dependent_id | Conf#job_conf.next_id],
									   State);
		net ->
			set_state_next_session_job(SessionJob#job_session_job.id,
									   lists:append(Conf#job_conf.child_id, Conf#job_conf.next_id),
									   State);
		job ->
			set_state_next_session_job(SessionJob#job_session_job.id,
									   Conf#job_conf.next_id,
									   State)
	end.
-spec set_state_next_session_job(session_id(), [{group_id(), job_id()}], job_state()) -> 
		  ok | {error, term()}.
set_state_next_session_job(_, [], State) ->
	ok;
set_state_next_session_job(SessionID, [{GroupID, JobID} | T], State) ->
	lager:info("~p", [{SessionID, GroupID, JobID, State, T}]),
	case kesto_job_session_info:get(SessionID,
									GroupID, 
									JobID) of
		{ok, SessionJob} ->
			set_state_next_session_job(SessionJob, State),
			NewSessionJob = case State == wait of
								true ->
									SessionJob#job_session_job{state=State,
															   end_state=undefined,
															   end_value=undefined,
															   start_timestamp=undefined,
															   end_timestamp=undefined};
								false ->
									SessionJob#job_session_job{state=State}
							end,
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					set_state_next_session_job(SessionID, T, State);
				{error, _Error} -> {error, _Error}
			end;
		{error, _Error} -> {error, _Error}
	end.

%% @doc セッションジョブの親セッションジョブの実行状態を設定する。
-spec set_state_parent_session_job(job_session_job(), job_state()) -> 
		  ok | {error, term()}.
set_state_parent_session_job(SessionJob, State) ->
	lager:info("~p", [{SessionJob, State}]),
	case is_root_jobnet(SessionJob) of
		true ->
			% セッションジョブが、ジョブネットの最初の場合
			case kesto_job_session_info:get(SessionJob#job_session_job.id) of
				{ok, Session} ->
					NewSession = case State == wait of
									 true ->
										 Session#job_session{state=State,
															 start_timestamp=undefined,
															 end_timestamp=undefined};
									 false ->
										 Session#job_session{state=State}
								 end,
					kesto_job_session_info:update(NewSession);
				{error, _Error} -> {error, _Error}
			end;
		false -> 
			NewSessionJob = case State == wait of
								true ->
									SessionJob#job_session_job{state=State,
															   end_state=undefined,
															   end_value=undefined,
															   start_timestamp=undefined,
															   end_timestamp=undefined};
								false ->
									SessionJob#job_session_job{state=State}
							end,
			case kesto_job_session_info:update(NewSessionJob) of
				ok ->
					set_state_parent_session_job(NewSessionJob, State);
				{error, _Error} -> {error, _Error}
			end
	end.
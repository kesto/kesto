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

-module(job_test_util).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).
-compile(export_all).

-define(CHECK_STATUE_INTERVAL, 3000).

check_session_state(SessionID, State) ->
	list(),
	case kesto_job_session_info:get(SessionID) of
		{ok, Session} ->
			case Session#job_session.state == State of
				true ->
					true;
				false ->
					timer:sleep(?CHECK_STATUE_INTERVAL),
					check_session_state(SessionID, State)
			end;
		_ ->
			timer:sleep(?CHECK_STATUE_INTERVAL),
			check_session_state(SessionID, State)
	end.
check_session_state(SessionID, GroupID, JobID, State, EndState) ->
	list(),
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			case SessionJob#job_session_job.state == State of
				true ->
					SessionJob#job_session_job.end_state == EndState;
				false ->
					timer:sleep(?CHECK_STATUE_INTERVAL),
					check_session_state(SessionID, GroupID, JobID, State, EndState)
			end;
		_ ->
			timer:sleep(?CHECK_STATUE_INTERVAL),
			check_session_state(SessionID, GroupID, JobID, State, EndState)
	end.
check_session_state(SessionID, GroupID, JobID, FacilityID, State, EndValue) ->
	list(),
	case kesto_job_session_info:get(SessionID, GroupID, JobID, FacilityID) of
		{ok, SessionNode} ->
			case SessionNode#job_session_node.state == State of
				true ->
					SessionNode#job_session_node.end_value == EndValue;
				false ->
					timer:sleep(?CHECK_STATUE_INTERVAL),
					check_session_state(SessionID, GroupID, JobID, FacilityID, State, EndValue)
			end;
		_ ->
			timer:sleep(?CHECK_STATUE_INTERVAL),
			check_session_state(SessionID, GroupID, JobID, FacilityID, State, EndValue)
	end.

list() ->
	{ok, List} = kesto_job_session_info:get_list(all, all),
	SortedList = lists:sort(fun(X, Y) ->
									kesto_demo:get_id(X) =< kesto_demo:get_id(Y)
							end,
							List),
	lager:info("---------------------------------------------------------"),
	print_list(SortedList),
	lager:info("---------------------------------------------------------").

print_list([]) ->
	ok;
print_list([SessionInfo | T]) ->
	print_job_session(SessionInfo),
	print_list(T).

print_job_session(SessionInfo) when is_record(SessionInfo, job_session) ->
	{SessionID,
	 JobID, 
	 GroupID, 
	 State,
	 StaerTimestamp,
	 EndTimestamp} = {SessionInfo#job_session.id,
					  SessionInfo#job_session.job_id, 
					  SessionInfo#job_session.group_id, 
					  SessionInfo#job_session.state, 
					  SessionInfo#job_session.start_timestamp,
					  SessionInfo#job_session.end_timestamp},
	lager:info("job_session : ~s, ~s, ~s, ~p, ~p, ~p", [SessionID,
														GroupID, 
														JobID, 
														State,
														StaerTimestamp,
														EndTimestamp]);
print_job_session(SessionInfo) when is_record(SessionInfo, job_session_job) ->
	{SessionID,
	 JobID, 
	 GroupID, 
	 State,
	 EndValue,
	 EndState,
	 ChildEndState,
	 ChildEndValue,
	 DependentEndState,
	 PreviousEndState,
	 StaerTimestamp,
	 EndTimestamp} = {SessionInfo#job_session_job.id,
					  SessionInfo#job_session_job.job_id, 
					  SessionInfo#job_session_job.group_id, 
					  SessionInfo#job_session_job.state, 
					  SessionInfo#job_session_job.end_value, 
					  SessionInfo#job_session_job.end_state, 
					  SessionInfo#job_session_job.child_end_state,
					  SessionInfo#job_session_job.child_end_value,
					  SessionInfo#job_session_job.dependent_end_state,
					  SessionInfo#job_session_job.previous_end_state,
					  SessionInfo#job_session_job.start_timestamp,
					  SessionInfo#job_session_job.end_timestamp},
	lager:info("job_session_job : ~s, ~s, ~s, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p", [SessionID,
																					GroupID, 
																					JobID, 
																					State,
																					EndValue,
																					EndState,
																					ChildEndState,
																					ChildEndValue,
																					DependentEndState,
																					PreviousEndState,
																					StaerTimestamp,
																					EndTimestamp]);
print_job_session(SessionInfo) when is_record(SessionInfo, job_session_node) ->
	{SessionID,
	 JobID, 
	 GroupID, 
	 FacirityID,
	 State,
	 CmdState,
	 EndValue,
	 Message,
	 StaerTimestamp,
	 EndTimestamp} = {SessionInfo#job_session_node.id,
					  SessionInfo#job_session_node.job_id, 
					  SessionInfo#job_session_node.group_id, 
					  SessionInfo#job_session_node.facility#node.id, 
					  SessionInfo#job_session_node.state, 
					  SessionInfo#job_session_node.cmd_state, 
					  SessionInfo#job_session_node.end_value, 
					  SessionInfo#job_session_node.message, 
					  SessionInfo#job_session_node.start_timestamp,
					  SessionInfo#job_session_node.end_timestamp},
	lager:info("job_session_node : ~s, ~s, ~s, ~s, ~p, ~p, ~p, ~s, ~p, ~p", [SessionID, 
																			 GroupID, 
																			 JobID, 
																			 FacirityID,
																			 State,
																			 CmdState,
																			 EndValue,
																			 Message,
																			 StaerTimestamp,
																			 EndTimestamp]).

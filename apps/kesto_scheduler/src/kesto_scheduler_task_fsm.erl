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
%% @doc スケジュール定義チェックモジュール

-module(kesto_scheduler_task_fsm).
-behaviour(gen_fsm).

-include("kesto_scheduler.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, lager_transform}]).

%% API
-export([
		 start_link/3, 
		 check/1
		]).

%% Callbacks
-export([
		 init/1, 
		 code_change/4, 
		 handle_event/3, 
		 handle_info/3,
		 handle_sync_event/4, 
		 terminate/3
		]).

%% States
-export([
		 prepare/2,
		 get_conf/2,
		 get_next_time/2,
		 check_next_time/2,
		 execute/2, 
		 waiting/2,
		 prepare_terminate/2
		]).

-define(RETRY, 1).

-define(DEFAULT_TIMEOUT, 1000).

-record(state, {req_id,
				from,
				target_id,
				preflist,
				retry,
				conf,
				last,
				next,
				timeout=?DEFAULT_TIMEOUT}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, TargetID) ->
	gen_fsm:start_link(?MODULE, [ReqID, From, TargetID], []).

check(TargetID) ->
	lager:debug("~p", [TargetID]),
	ReqID = make_reqid(),
	kesto_scheduler_task_fsm_sup:start_check_fsm([ReqID, self(), TargetID]),
	{ok, ReqID}.

%% Intiailize state data.
init([ReqID, From, TargetID]) ->
	lager:debug("~p", [{ReqID, From, TargetID}]),
	process_flag(trap_exit, true),
	global:register_name(node(), self()),
	StateData = #state{req_id=ReqID,
					   from=From,
					   target_id=TargetID,
					   retry=0},
	{ok, prepare, StateData, 0}.

%% @doc Calculate the Preflist.
prepare(timeout, StateData=#state{target_id=TargetID}) ->
	lager:debug("~p", [StateData]),
	
	case global:set_lock({TargetID, self()}, [node()|nodes()], 1) of
		true ->
			DocIdx = riak_core_util:chash_key({<<"kesto_scheduler_check">>,
											   list_to_binary(TargetID)}),
			Prelist = riak_core_apl:get_apl(DocIdx, 1, kesto_scheduler),
			NewStateData = StateData#state{preflist=Prelist},
%% 			kesto_scheduler_task_vnode:set(Prelist, TargetID, self(), node()),
			{next_state, get_conf, NewStateData, 0};
		false ->
			{next_state, prepare_terminate, StateData, 0}
	end.

%% @doc Execute the get reqs.
get_conf(timeout, StateData=#state{target_id=TargetID,
								   retry=Retry,
								   conf=Conf}) ->
	lager:debug("~p", [StateData]),
	case kesto_scheduler_conf:get(TargetID) of
		{ok, NewConf} ->
			case Conf == NewConf of
				true ->
					{next_state, check_next_time, StateData, 0};
				false ->
					NewStateData = StateData#state{conf=NewConf},
					{next_state, get_next_time, NewStateData, 0}
			end;
		{error, notfound} ->
			case Retry of
				0 ->
					NewStateData = StateData#state{retry=Retry + 1},
					{next_state, waiting, NewStateData, 1000};
				?RETRY ->
					{next_state, prepare_terminate, StateData}
			end
	end.

get_next_time(timeout, StateData=#state{conf=Conf}) ->
	lager:debug("~p", [StateData]),
	
	Now = calendar:local_time(),
	{_, {_, Minutes, Seconds}} = Now,
	
	Cycle = Conf#scheduler_conf.cycle,
	Start = Minutes * 60 + Seconds,
	End = Start + Cycle * 2,
	Plus = find_start(
			 [X || X <- lists:seq(Start, End), X rem Cycle == 0], 
			 Start),
	
	Next = calendar:gregorian_seconds_to_datetime(
			 calendar:datetime_to_gregorian_seconds(
			   Now) - Start + Plus),
	
	NewStateData = StateData#state{last=Now,
								   next=Next},
	{next_state, check_next_time, NewStateData, 0}.

find_start([], _Time) ->
	60 * 60;
find_start([Seconds | T], Time) ->
	if
		Seconds > Time -> Seconds;
		true -> find_start(T, Time)
	end.

check_next_time(timeout, StateData=#state{conf=Conf,
										  next=Next}) ->
	lager:debug("~p", [StateData]),
	case Next < calendar:local_time() of
		true ->
			NewNext = calendar:gregorian_seconds_to_datetime(
						calendar:datetime_to_gregorian_seconds(Next) + 
							Conf#scheduler_conf.cycle),
			NewStateData = StateData#state{last=Next,
										   next=NewNext},
			{next_state, execute, NewStateData, 0};
		false ->
			{next_state, waiting, StateData, 0}
	end.

%% @doc Execute the get reqs.
execute(timeout, StateData=#state{target_id=TargetID,
								  preflist=Prelist,
								  conf=Conf}) ->
	lager:debug("~p", [tateData]),
	
	case Conf#scheduler_conf.target_type of
		monitor ->
			kesto_monitor_vnode:run(Prelist, TargetID),
			{next_state, waiting, StateData, 0};
		_ ->
			{next_state, prepare_terminate, StateData, 0}
	end.

%% @doc Execute the get reqs.
waiting(timeout, StateData) ->
	lager:debug("~p", [StateData]),
	{next_state, get_conf, StateData, 1000}.

%% @doc Execute the get reqs.
prepare_terminate(timeout, StateData=#state{target_id=TargetID}) ->
	lager:debug("~p", [StateData]),
%% 	kesto_scheduler_task_vnode:delete(Prelist, TargetID),
	global:del_lock({TargetID, self()}),
	{stop, normal, StateData}.

make_reqid() -> 
	erlang:phash2(erlang:now()).

handle_info(_Info, _StateName, StateData) ->
	lager:debug("~p", [{_Info, _StateName, StateData}]),
	{stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
	lager:debug("~p", [{_Event, _StateName, StateData}]),
	{stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
	lager:debug("~p", [{_Event, _From, _StateName, StateData}]),
	{stop, badmsg, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	lager:debug("~p", [{_OldVsn, StateName, StateData, _Extra}]),
	{ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	lager:debug("~p", [{_Reason, _StateName, _StateData}]),
	global:unregister_name(node()),
	ok.

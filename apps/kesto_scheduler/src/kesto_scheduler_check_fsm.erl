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
%% @doc スケジュール定義チェック用gen_fsmモジュール

-module(kesto_scheduler_check_fsm).
-behaviour(gen_fsm).

-include("kesto_scheduler.hrl").

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
		 get_conf/2, 
		 check/2
		]).

-record(state, {conf,
				next,
				interval,
				cycle,
				offset}).

%% @spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%% @doc スケジュール定義チェック用gen_fsmを開始。
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%% @spec init([]) -> {ok, atom(), #state{}, interger()}.
%% @doc 初期化処理
-spec init([]) -> {ok, atom(), #state{}, integer()}.
init([]) ->
	process_flag(trap_exit, true),
	Interval = kesto_scheduler_config:check_interval(),
	Cycle = kesto_scheduler_config:check_cycle(),
	Offset = kesto_scheduler_config:check_offset(),
	StateData=#state{interval=Interval,
					 cycle=Cycle,
					 offset=Offset},
	{ok, prepare, StateData, 5000}.

prepare(timeout, StateData=#state{interval=Interval,
								  offset=Offset}) ->
	lager:debug("~p", [StateData]),
	Next = get_next_time(Interval, Offset),
	NewStateData = StateData#state{next=Next},
	{next_state, get_conf, NewStateData, 0}.

waiting(timeout, StateData=#state{cycle=Cycle}) ->
	lager:debug("~p", [StateData]),
	{next_state, check_next_time, StateData, Cycle * 1000}.

check_next_time(timeout, StateData=#state{interval=Interval,
										  next=Next}) ->
	lager:debug("~p", [StateData]),
	case Next < calendar:local_time() of
		true ->
			NewNext = calendar:gregorian_seconds_to_datetime(
						calendar:datetime_to_gregorian_seconds(Next) + Interval),
			NewStateData = StateData#state{next=NewNext},
			{next_state, get_conf, NewStateData, 0};
		false ->
			{next_state, check, StateData, 0}
	end.

get_conf(timeout, StateData) ->
	lager:debug("~p", [StateData]),
	Conf = case kesto_scheduler_conf:get_id_list(simple) of
			   {ok, List} -> List;
			   {error, _Error} -> []
		   end,
	NewStateData = StateData#state{conf=Conf},
	{next_state, check, NewStateData, 0}.

check(timeout, StateData=#state{conf=Conf}) ->
	lager:debug("~p", [StateData]),
	check_schedule_conf(Conf),
	{next_state, waiting, StateData, 0}.

check_schedule_conf([]) ->
	ok;
check_schedule_conf([ID | T]) ->
	kesto_scheduler_task_fsm:check(ID),
	check_schedule_conf(T).

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
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
%% @doc セッション操作モジュール

-module(kesto_job_session_operator).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

-export([
		 operate/4
		]).

%% @doc セッション管理を開始する。
-spec operate(job_operation_args(), operation_id(), job_operation(), job_end_value()) -> ok | {error, term()}.
operate(Args={_SessionID}, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{Args, OperationID, Operation, EndValue}]),
	init(kesto_job_session, Args, OperationID, Operation, EndValue);
operate(Args={SessionID, GroupID, JobID}, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{Args, OperationID, Operation, EndValue}]),
	case kesto_job_session_info:get(SessionID, GroupID, JobID) of
		{ok, SessionJob} ->
			Conf = SessionJob#job_session_job.conf,
			case Conf#job_conf.type of
				net ->
					init(kesto_job_session_net, Args, OperationID, Operation, EndValue);
				job ->
					init(kesto_job_session_job, Args, OperationID, Operation, EndValue);
				judgment ->
					init(kesto_job_session_judgment, Args, OperationID, Operation, EndValue)
			end;
		{error, _Error} ->
			lager:error("セッションジョブ情報の取得に失敗したため、FSMを終了します。: ~p", [_Error]),
			{error, notfound}
	end;
operate(Args={_SessionID, _GroupID, _JobID, _FacilityID}, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{Args, OperationID, Operation, EndValue}]),
	init(kesto_job_session_node, Args, OperationID, Operation, EndValue).

-spec init(term(), job_operation_args(), operation_id(), job_operation(), job_end_value()) -> 
		  ok | {error, term()}.
init(Module, Args, OperationID, Operation, EndValue) ->
	lager:debug("~p", [{Module, Args, OperationID, Operation, EndValue}]),
	process_flag(trap_exit, true),
	case Module:init(Args, Operation) of
		{ok, State} ->
			NewState = State#job_session_state{module=Module,
											   operation_id=OperationID, 
											   operation=Operation,
											   operation_end_value=EndValue},
			exec(Operation, NewState);
		{error, _Error} ->
			lager:error("エラーが発生したため停止します。: ~p", [_Error]),
			{error, _Error}
	end.

-spec exec(job_operate_method(), job_session_state()) -> 
		  ok | {ok, job_operate_method(), job_session_state()} | {error, term()}.
exec(terminate, State=#job_session_state{module=Module}) ->
	lager:debug("~p", [State]),
	case Module:terminate(State) of
		ok -> ok;
		{error, _Error} ->
			lager:error("エラーが発生したため停止します。: ~p", [_Error]),
			{error, _Error}
	end;
exec(Method, State=#job_session_state{module=Module}) ->
	lager:debug("~p", [State]),
	case Module:Method(State) of
		{ok, NextMethod, NewState} ->
			exec(NextMethod, NewState);
		{error, _Error} ->
			lager:error("エラーが発生したため停止します。: ~p", [_Error]),
			exec(terminate, State)
	end.

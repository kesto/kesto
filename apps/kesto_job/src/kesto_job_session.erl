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
%% @doc セッション管理モジュール

-module(kesto_job_session).

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
-spec init({session_id()}, job_operation()) -> {ok, job_session_state()} | {error, term()}.
init({SessionID}, Operation) ->
	lager:debug("~p", [{SessionID}]),
	Retry = case Operation of
				start -> infinity;
				_ -> ?RETRY_SESSION_LOCK_MAX
			end,
	global:register_name(node(), self()),
	case global:set_lock({SessionID, self()}, [node()|nodes()], Retry) of
		true ->
			case kesto_job_session_info:get(SessionID) of
				{ok, SessionInfo} ->
					State = #job_session_state{session_info=SessionInfo},
					{ok, State};
				{error, _Error} ->
					lager:error("セッション情報の取得に失敗したため、FSMを終了します。: ~p", [{SessionID, _Error}]),
					{error, notfound}
			end;
		false ->
			lager:error("セッションIDによるロックの取得に失敗したため、FSMを終了します。: ~p", [SessionID]),
			{error, lock_error}
	end.

%% @doc セッションを開始する。
-spec start(job_session_state()) -> {ok, atom(), job_session_state()} | {error, term()}.
start(State=#job_session_state{session_info=Session}) ->
	lager:debug("~p", [State]),
	NewSession = Session#job_session{state=running,
									 start_timestamp=calendar:local_time()},
	case kesto_job_session_info:update(NewSession) of
		ok ->
			lager:info("セッション[~p]を開始しました。", 
					   [{Session#job_session.id}]),
			lager:debug("~p", [{NewSession#job_session.id, 
							   NewSession#job_session.group_id, 
							   NewSession#job_session.job_id}]),
			Preflist = kesto_job_session_util:get_preflist(),
			kesto_job_vnode:operate_session(Preflist, 
											NewSession#job_session.id, 
											NewSession#job_session.group_id, 
											NewSession#job_session.job_id, 
											undefined, start, 0),
			NewState = State#job_session_state{session_info=NewSession},
			{ok, check, NewState};
		{error, _Error} ->
			lager:error("セッション情報の更新に失敗しました。: ~p", [{NewSession#job_session.id,_Error}]),
			{error, _Error}
	end.

%% @doc セッション情報を確認し、セッションの制御を行う。
-spec check(job_session_state()) -> {true | false, atom(), job_session_state()} | {error, term()}.
check(State=#job_session_state{session_info=Session}) ->
	lager:debug("~p", [State]),
	case kesto_job_session_info:get(Session#job_session.id, 
									Session#job_session.group_id, 
									Session#job_session.job_id) of
		{ok, SessionJob} ->
			lager:debug("~p", [{SessionJob#job_session_job.id, 
							   SessionJob#job_session_job.group_id, 
							   SessionJob#job_session_job.job_id}]),
			Preflist = kesto_job_session_util:get_preflist(),
			kesto_job_vnode:operate_session(Preflist, 
											SessionJob#job_session_job.id, 
											SessionJob#job_session_job.group_id, 
											SessionJob#job_session_job.job_id,
											undefined, check, 0),
			case SessionJob#job_session_job.state of 
				'end' ->
					{ok, 'end', State};
				changed -> 
					{ok, 'end', State};
				_ ->
					{ok, terminate, State}
			end;
		{error, _Error} ->
			lager:error("セッションジョブの取得に失敗しました。: ~p", 
						[{Session#job_session.id, 
						  Session#job_session.group_id, 
						  Session#job_session.job_id,
						  _Error}]),
			{error, _Error}
	end.

%% @doc セッションを終了する。
-spec 'end'(job_session_state()) -> {ok, atom(), job_session_state()} | {error, term()}.
'end'(State=#job_session_state{session_info=Session}) ->
	lager:debug("~p", [State]),
	NewSession = Session#job_session{state='end',
									 end_timestamp=calendar:local_time()},
	case kesto_job_session_info:update(NewSession) of
		ok ->
			lager:info("セッション[~p]を終了しました。", 
					   [{Session#job_session.id}]),
			NewState = State#job_session_state{session_info=NewSession},
			{ok, next, NewState};
		{error, _Error} ->
			lager:error("セッション情報の更新に失敗しました。: ~p", 
						[{NewSession#job_session.id,_Error}]),
			{error, _Error}
	end.

%% @doc 後続のセッションジョブを開始する。
-spec next(job_session_state()) -> {ok, atom(), job_session_state()} | {error, term()}.
next(State) ->
	lager:debug("~p", [State]),
	{ok, terminate, State}.

%% @doc 終了の準備をする。
-spec terminate(job_session_state()) -> ok.
terminate(State=#job_session_state{session_info=Session}) ->
	lager:debug("~p", [State]),
	global:del_lock({Session#job_session.id, self()}),
	global:unregister_name(node()),
	ok.

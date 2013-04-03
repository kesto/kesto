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
%% @doc セッションノード管理モジュール

-module(kesto_job_session_node).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile([{parse_transform, lager_transform}]).

%% States
-export([
		 init/2,
		 start/1,
		 stop/1,
		 change/1,
		 restart/1,
		 force_stop/1,
		 'end'/1,
		 next/1,
		 terminate/1
		]).

%% @doc ステートを初期化する。
-spec init({session_id(), group_id(), job_id(), facility_id()}, job_operation()) -> {ok, job_session_state()} | {error, term()}.
init({SessionID, GroupID, JobID, FacilityID}, Operation) ->
	lager:debug("~p", [{SessionID, GroupID, JobID, FacilityID}]),
	global:register_name(node(), self()),
	case global:set_lock({{SessionID, GroupID, JobID, FacilityID, Operation}, self()}, [node()|nodes()], infinity) of
		true ->
			case kesto_job_session_info:get(SessionID, GroupID, JobID, FacilityID) of
				{ok, SessionNode} ->
					case kesto_job_session_info:get(SessionID, GroupID, JobID) of
						{ok, SessionJob} ->
							State = #job_session_state{session_info=SessionNode,
													   command=SessionJob#job_session_job.conf#job_conf.command},
							{ok, State};
						{error, _Error} ->
							lager:error("セッションジョブ情報の取得に失敗したため、FSMを終了します。: ~p", [{SessionID, GroupID, JobID, _Error}]),
							{error, notfound}
					end;
				{error, _Error} ->
					lager:error("セッションノード情報の取得に失敗したため、FSMを終了します。: ~p", [{SessionID, GroupID, JobID, FacilityID, _Error}]),
					{error, notfound}
			end;
		false ->
			lager:error("セッションID、グループID、ジョブID、ファシリティIDによるロックの取得に失敗したため、FSMを終了します。: ~p", [{SessionID, GroupID, JobID, FacilityID}]),
			{error, lock_error}
	end.

%% @doc セッションノードを開始する。
-spec start(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
start(State=#job_session_state{session_info=SessionNode}) ->
	lager:debug("~p", [State]),
	NewSessionNode = SessionNode#job_session_node{state=running,
												  cmd_state=running,
												  start_timestamp=calendar:local_time()},
	case kesto_job_session_info:update(NewSessionNode) of
		ok ->
			lager:info("ジョブノード[~p]を実行中にしました。", 
					   [{SessionNode#job_session_node.id, 
						 SessionNode#job_session_node.group_id, 
						 SessionNode#job_session_node.job_id,
						 SessionNode#job_session_node.facility#node.id}]),
			execute_start_command(State#job_session_state{session_info=NewSessionNode});
		{error, _Error} -> {error, _Error}
	end.

%% @doc SSH接続オプションを取得する。
-spec get_ssh_options(node()) -> [term()].
get_ssh_options(Facility) ->
	[{user, Facility#node.ssh_user}, 
	 {password, Facility#node.ssh_password}, 
	 {silently_accept_hosts, true},
	 {connect_timeout, kesto_core_config:ssh_client_connect_timeout()},
	 {ip_v6_disabled, false}].

%% @doc 起動コマンドを実行する。
-spec execute_start_command(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
execute_start_command(State=#job_session_state{session_info=SessionNode,
											   command=Command}) ->
	lager:debug("~p", [State]),
	Facility = SessionNode#job_session_node.facility,
	%% NewCmd = kesto_core_replace:replace(Cmd, NotifyInfo),
	lager:info("ジョブノード[~p]にてコマンド[~p]を実行します。", 
			   [{SessionNode#job_session_node.id, 
				 SessionNode#job_session_node.group_id, 
				 SessionNode#job_session_node.job_id,
				 SessionNode#job_session_node.facility#node.id},
				Command#job_conf_command.cmd_start_command]),
	NewSessionNode = case execute_cmd(Facility#node.ipv4 ++ Facility#node.ipv6, 
									  Facility#node.ssh_port, 
									  get_ssh_options(Facility), 
									  Command#job_conf_command.cmd_start_command, 
									  kesto_job_config:ssh_exec_timeout()) of
						 {ok, Data, ExitStatus} -> 
							 lager:info("ジョブノード[~p]にてコマンド[~p]が終了しました。: ~p", 
										[{SessionNode#job_session_node.id, 
										  SessionNode#job_session_node.group_id, 
										  SessionNode#job_session_node.job_id,
										  SessionNode#job_session_node.facility#node.id},
										 Command#job_conf_command.cmd_start_command,
										 {Data, ExitStatus}]),
							 lager:debug("~s, ~p", [Data, ExitStatus]),
							 SessionNode#job_session_node{cmd_state='end',
														  end_value=ExitStatus,
														  message=Data,
														  end_timestamp=calendar:local_time()};
						 {error, _Error1} -> 
							 lager:error("起動コマンドの実行に失敗しました。: ~p", 
										 [{SessionNode#job_session_node.id,
										   SessionNode#job_session_node.group_id,
										   SessionNode#job_session_node.job_id,
										   Facility#node.id,_Error1}]),
							 case Command#job_conf_command.cmd_error_end_flg of
								 true ->
									 SessionNode#job_session_node{cmd_state='end',
																  end_value=Command#job_conf_command.cmd_error_end_value,
																  end_timestamp=calendar:local_time()};
								 false ->
									 SessionNode#job_session_node{cmd_state=error,
																  end_timestamp=calendar:local_time()}
							 end
					 end,
	NewState = State#job_session_state{session_info=NewSessionNode},
	{ok, 'end', NewState}.

%% @doc セッションノードを停止する。
-spec stop(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
stop(State=#job_session_state{session_info=SessionNode,
							  operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case SessionNode#job_session_node.state == running of
		true ->
			NewSessionNode = SessionNode#job_session_node{state=stopping,
														  cmd_state=running,
														  start_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionNode) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					execute_stop_command(State#job_session_state{session_info=NewSessionNode});
				{error, _Error} -> {error, _Error}
			end;
		false ->
			NewSessionNode = SessionNode#job_session_node{state=stopping,
														  cmd_state=stoped,
														  end_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionNode) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, 'end', State#job_session_state{session_info=NewSessionNode}};
				{error, _Error} -> {error, _Error}
			end
	end.

%% @doc 停止コマンドを実行する。
-spec execute_stop_command(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
execute_stop_command(State=#job_session_state{session_info=SessionNode,
											  command=Command}) ->
	lager:debug("~p", [State]),
	
	Facility = SessionNode#job_session_node.facility,
	%% NewCmd = kesto_core_replace:replace(Cmd, NotifyInfo),
	NewSessionNode = case execute_cmd(Facility#node.ipv4 ++ Facility#node.ipv6, 
									  Facility#node.ssh_port, 
									  get_ssh_options(Facility), 
									  Command#job_conf_command.cmd_stop_command, 
									  kesto_job_config:ssh_exec_timeout()) of
						 {ok, Data, ExitStatus} -> 
							 lager:debug("~s, ~p", [Data, ExitStatus]),
							 SessionNode#job_session_node{cmd_state=stoped,
														  end_value=ExitStatus,
														  message=Data,
														  end_timestamp=calendar:local_time()};
						 {error, _Error1} -> 
							 lager:error("起動コマンドの実行に失敗しました。: ~p", 
										 [{SessionNode#job_session_node.id,
										   SessionNode#job_session_node.group_id,
										   SessionNode#job_session_node.job_id,
										   Facility#node.id,_Error1}]),
							 case Command#job_conf_command.cmd_error_end_flg of
								 true ->
									 SessionNode#job_session_node{cmd_state=stoped,
																  end_value=Command#job_conf_command.cmd_error_end_value,
																  end_timestamp=calendar:local_time()};
								 false ->
									 SessionNode#job_session_node{cmd_state=error,
																  end_timestamp=calendar:local_time()}
							 end
					 end,
	NewState = State#job_session_state{session_info=NewSessionNode},
	{ok, 'end', NewState}.

%% @spec exec_cmd([string()], integer(), [term()], string(), integer()) -> ok.
%% @doc コマンドを実行する。
-spec execute_cmd([string()], integer(), [term()], string(), integer()) -> {ok, string(), integer()} | {error, term()}.
execute_cmd([], _Port, _Opts, _Cmd, _Timeout) ->
	{error, nonexec};
execute_cmd([IpAddress | T], Port, Opts, Cmd, Timeout) ->
	lager:debug("~p", [{IpAddress, Port, Opts, Cmd, Timeout}]),
	Preflist = kesto_job_session_util:get_preflist(),
	case kesto_core_command_vnode:exec(Preflist, IpAddress, Port, Opts, Cmd, Timeout) of
		{ok, Data, ExitStatus} ->
			{ok, Data, ExitStatus};
		{error, failure} ->
			{error, failure};
		{error, timeout} ->
			{error, timeout};
		{error, notconnect} ->
			execute_cmd(T, Port, Opts, Cmd, Timeout)
	end.

%% @doc セッションノードを強制終了する。
-spec restart(job_session_state()) -> 
		  {ok, job_operate_method(), job_session_state()} | {error, term()}.
restart(State=#job_session_state{session_info=SessionNode,
								 operation_id=OperationID}) ->
	lager:debug("~p", [State]),
	case (SessionNode#job_session_job.state == stopped)
			 or (SessionNode#job_session_job.state == 'end')
			 or (SessionNode#job_session_job.state == changed)
			 or (SessionNode#job_session_job.state == error) of
		true ->
			NewSessionNode = SessionNode#job_session_node{state=wait,
														  cmd_state=undefined,
														  end_value=undefined,
														  message=undefined,
														  start_timestamp=undefined,
														  end_timestamp=undefined},
			case kesto_job_session_info:update(NewSessionNode) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, start, State#job_session_state{session_info=NewSessionNode}};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションノードを強制終了する。
-spec force_stop(job_session_state()) -> 
		  {ok, job_operate_method(), job_session_state()} | {error, term()}.
force_stop(State=#job_session_state{session_info=SessionNode,
									operation_id=OperationID,
									operation_end_value=EndValue}) ->
	lager:debug("~p", [State]),
	case SessionNode#job_session_node.state == stopping of
		true ->
			NewSessionNode = SessionNode#job_session_node{cmd_state='end',
														  end_value=EndValue,
														  start_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionNode) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, 'end', State#job_session_state{session_info=NewSessionNode}};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションノードの終了値を変更する。
-spec change(job_session_state()) -> 
		  {ok, job_operate_method(), job_session_state()} | {error, term()}.
change(State=#job_session_state{session_info=SessionNode,
								operation_id=OperationID,
								operation_end_value=EndValue}) ->
	lager:debug("~p", [State]),
	case (SessionNode#job_session_node.state == stoped) 
			 or (SessionNode#job_session_node.state == error) of
		true ->
			NewSessionNode = SessionNode#job_session_node{cmd_state=changed,
														  end_value=EndValue,
														  start_timestamp=calendar:local_time()},
			case kesto_job_session_info:update(NewSessionNode) of
				ok ->
					kesto_job_session_operation:setState(OperationID, 
														 'end',
														 "",
														 calendar:local_time()),
					{ok, 'end', State#job_session_state{session_info=NewSessionNode}};
				{error, _Error} -> {error, _Error}
			end;
		false ->
			kesto_job_session_operation:setState(OperationID, 
												 'end',
												 "state unmatch",
												 calendar:local_time()),
			{ok, terminate, State}
	end.

%% @doc セッションノードを終了する。
-spec 'end'(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
'end'(State=#job_session_state{session_info=SessionNode,
							   operation=Operation}) ->
	lager:debug("~p", [State]),
	Facility = SessionNode#job_session_node.facility,
	case kesto_job_session_info:get(SessionNode#job_session_node.id,
									SessionNode#job_session_node.group_id,
									SessionNode#job_session_node.job_id,
									Facility#node.id) of
		{ok, Last} ->
			case Operation of
				start ->
					case Last#job_session_node.state == running of
						true ->
							NewSessionNode = SessionNode#job_session_node{state=SessionNode#job_session_node.cmd_state},
							case kesto_job_session_info:update(NewSessionNode) of
								ok ->
									lager:info("ジョブノード[~p]を終了しました。", 
											   [{SessionNode#job_session_node.id, 
												 SessionNode#job_session_node.group_id, 
												 SessionNode#job_session_node.job_id,
												 SessionNode#job_session_node.facility#node.id}]),
									NewState = State#job_session_state{session_info=NewSessionNode},
									{ok, next, NewState};
								{error, _Error} -> {error, _Error}
							end;
						false -> {ok, terminate, State}
					end;
				stop ->
					case Last#job_session_node.state == stopping of
						true ->
							NewSessionNode = SessionNode#job_session_node{state=SessionNode#job_session_node.cmd_state},
							case kesto_job_session_info:update(NewSessionNode) of
								ok ->
									NewState = State#job_session_state{session_info=NewSessionNode},
									{ok, next, NewState};
								{error, _Error} -> {error, _Error}
							end;
						false -> {ok, terminate, State}
					end;
				change ->
					case (Last#job_session_node.state == stoped) 
							 or (Last#job_session_node.state == error) of
						true ->
							NewSessionNode = SessionNode#job_session_node{state=SessionNode#job_session_node.cmd_state},
							case kesto_job_session_info:update(NewSessionNode) of
								ok ->
									NewState = State#job_session_state{session_info=NewSessionNode},
									{ok, next, NewState};
								{error, _Error} -> {error, _Error}
							end;
						false -> {ok, terminate, State}
					end
			end;
		{error, _Error} -> {error, _Error}
	end.

%% @doc 後続のセッションジョブを開始する。
-spec next(job_session_state()) -> {ok, job_operate_method(), job_session_state()} | {error, term()}.
next(State=#job_session_state{session_info=SessionNode}) ->
	lager:debug("~p", [State]),
	%% 	Preflist = kesto_job_session_util:get_preflist(),
	%% 	kesto_job_vnode:operate_session(Preflist, 
	%% 									SessionNode#job_session_node.id, 
	%% 									SessionNode#job_session_node.group_id, 
	%% 									SessionNode#job_session_node.job_id,
	%% 									undefined, check, 0),
	{ok, terminate, State}.

%% @doc 終了の準備をする。
-spec terminate(job_session_state()) -> ok.
terminate(State=#job_session_state{session_info=SessionNode,
								   operation=Operation}) ->
	lager:debug("~p", [State]),
	global:del_lock({{SessionNode#job_session_node.id, 
					  SessionNode#job_session_node.group_id, 
					  SessionNode#job_session_node.job_id, 
					  SessionNode#job_session_node.facility#node.id,
					  Operation}, 
					 self()}),
	global:unregister_name(node()),
	ok.

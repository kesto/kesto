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
%% @doc kesto_poller's application module.

-module(kesto_core_command).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 notify/2,
		 exec/5
		]).

-define(SSH_PACKET_SIZE, 32768).
-define(SSH_WINDOW_SIZE, 4*?SSH_PACKET_SIZE).

%% @doc コマンドを実行する。
-spec notify(string(), notify_info()) -> ok | {error, atom()} | {error, term()}.
notify(NotifyID, NotifyInfo) ->
	lager:info("~p", [{NotifyID, NotifyInfo}]),
	
	Conf = case kesto_core_notify:get(NotifyID) of
			   {ok, Obj1} -> Obj1;
			   {error, _Error1} -> notfound
		   end,
	case is_record(Conf, notify_conf) of
		true ->
			case is_record(Conf#notify_conf.conf, notify_command_conf) of
				true ->
					exec_cmd(NotifyInfo, Conf#notify_conf.conf);
				false ->
					lager:error("通知定義[~s]にコマンド通知定義が含まれていないため、コマンド通知を行いません。", [NotifyID]),
					{error, notfound}
			end;
		false ->
			lager:error("通知定義[~s]が存在しないため、メール通知を行いません。", [NotifyID]),
			{error, notfound}
	end.

%% @doc 通知情報、コマンド通知定義を元にコマンドを実行する。
-spec exec_cmd(notify_info(), notify_command_conf()) -> ok.
exec_cmd(NotifyInfo, CommandConf) ->
	case kesto_core_repository:get(NotifyInfo#notify_info.facility_id) of
		{ok, Node} -> 
			case check_priority(NotifyInfo, CommandConf) of
				{true, User, Password, Cmd, Timeout} ->
					Opts = [{user, User}, 
							{password, Password}, 
							{silently_accept_hosts, true},
							{connect_timeout, kesto_core_config:ssh_client_connect_timeout()},
							{ip_v6_disabled, false}],
					NewCmd = kesto_core_replace:replace(Cmd, NotifyInfo),
					case exec_cmd(Node#node.ipv4 ++ Node#node.ipv6, Node#node.ssh_port, Opts, NewCmd, Timeout) of
						{ok, _Data, _ExitStatus} -> ok;
						{error, _Error2} -> {error, nonexec}
					end;
				false ->
					lager:debug("重要度別通知フラグにチェックが無いため、コマンド通知を行いません。: ~p, ~p", [NotifyInfo, CommandConf]),
					{error, nocheck}
			end;
		{error, _Error1} ->
			lager:error("ノード[~s]が存在しないため、コマンド通知を行いません。", [NotifyInfo#notify_info.facility_id]),
			{error, notfound}
	end.

%% @doc コマンドを実行する。
-spec exec_cmd([string()], integer(), [term()], string(), integer()) -> ok.
exec_cmd([], _Port, _Opts, _Cmd, _Timeout) ->
	{error, nonexec};
exec_cmd([IpAddress | T], Port, Opts, Cmd, Timeout) ->
	case exec(IpAddress, Port, Opts, Cmd, Timeout) of
		{ok, Data, ExitStatus} ->
			lager:info("コマンド通知を実施。: ~s, ~p, ~p, ~s, ~p", [IpAddress, Port, Opts, Cmd, Timeout]),
			{ok, Data, ExitStatus};
		{error, failure} ->
			lager:error("コマンド通知に失敗しました。: ~s, ~p, ~p, ~s, ~p", [IpAddress, Port, Opts, Cmd, Timeout]),
			{error, failure};
		{error, notconnect} ->
			exec_cmd(T, Port, Opts, Cmd, Timeout)
	end.

%% @doc 通知情報、コマンド通知定義を元にコマンドを実行する。
-spec check_priority(notify_info(), notify_command_conf()) -> {true, string(), string(), string(), integer()} | false.
check_priority(_NotifyInfo=#notify_info{priority=Priority}, CommandConf) ->
	case Priority of
		info ->
			case CommandConf#notify_command_conf.info_enabled of
				true -> {true, 
						 CommandConf#notify_command_conf.info_user, 
						 CommandConf#notify_command_conf.info_password, 
						 CommandConf#notify_command_conf.info_cmd, 
						 CommandConf#notify_command_conf.info_cmd_timeout};
				false -> false
			end;
		warning ->
			case CommandConf#notify_command_conf.warning_enabled of
				true -> {true, 
						 CommandConf#notify_command_conf.warning_user, 
						 CommandConf#notify_command_conf.warning_password, 
						 CommandConf#notify_command_conf.warning_cmd, 
						 CommandConf#notify_command_conf.warning_cmd_timeout};
				false -> false
			end;
		error ->
			case CommandConf#notify_command_conf.error_enabled of
				true -> {true, 
						 CommandConf#notify_command_conf.error_user, 
						 CommandConf#notify_command_conf.error_password, 
						 CommandConf#notify_command_conf.error_cmd, 
						 CommandConf#notify_command_conf.error_cmd_timeout};
				false -> false
			end;
		unknown ->
			case CommandConf#notify_command_conf.unknown_enabled of
				true -> {true, 
						 CommandConf#notify_command_conf.unknown_user, 
						 CommandConf#notify_command_conf.unknown_password, 
						 CommandConf#notify_command_conf.unknown_cmd, 
						 CommandConf#notify_command_conf.unknown_cmd_timeout};
				false -> false
			end
	end.

%% @doc ssh接続を実施する。
-spec connect(string(), integer(), [term()], integer()) -> {ok, pid(), term(), integer()} | {error, atom()} | {error, term()}.
connect(Host, Port, Opts, Timeout) ->
	case ssh:connect(Host, Port, Opts) of
		{ok, ConnectionRef} ->
			case ssh_connection:session_channel(ConnectionRef, 
												?SSH_WINDOW_SIZE,
												?SSH_PACKET_SIZE,
												Timeout) of
				{ok, ChannelId} ->
					{ok, ConnectionRef, ChannelId};
				{error, _Reason} ->
					lager:error("SSHセッションのチャネルのオープンに失敗しました。: ~p, ~s, ~p, ~p", [_Reason, Host, Port, Opts]),
					{error, _Reason}
			end;
		{error, _Reason} ->
			lager:error("SSHでの接続に失敗しました。: ~p, ~s, ~p, ~p", [_Reason, Host, Port, Opts]),
			{error, _Reason}
	end.

%% @doc ssh接続を切断する。
-spec disconnect(term(), integer()) -> ok.
disconnect(ConnectionRef, ChannelId) ->
	ssh_connection:close(ConnectionRef, ChannelId),
	ssh:close(ConnectionRef).

%% @doc コマンドを実行する。
-spec exec(string(), integer(), [term()], string(), integer()) ->
		  {ok, string(), integer()} | {error, term()}.
exec(Host, Port, Opts, Cmd, Timeout) ->
	lager:info("~p", [{Host, Port, Opts, Cmd}]),
	case connect(Host, Port, Opts, Timeout) of
		{ok, ConnectionRef, ChannelId} ->
			case ssh_connection:exec(ConnectionRef, ChannelId, Cmd, Timeout) of
				success ->
					case receive_exec_result("", empty, Timeout) of
						{ok, Data, ExitStatus} ->
							lager:debug("コマンドを実行しました。: ~s, ~p, ~p, ~s, ~p", [Host, Port, Opts, Cmd, Timeout]),
							disconnect(ConnectionRef, ChannelId),
							{ok, Data, ExitStatus};
						{error, timeout} ->
							lager:error("コマンドの実行に失敗しました。: ~s, ~p, ~p, ~s, ~p", [Host, Port, Opts, Cmd, Timeout]),
							disconnect(ConnectionRef, ChannelId),
							{error, timeout}
					end;
				failure ->
					lager:error("コマンドの実行に失敗しました。: ~s, ~p, ~p, ~s, ~p", [Host, Port, Opts, Cmd, Timeout]),
					disconnect(ConnectionRef, ChannelId),
					{error, failure}
			end;
		{error, _} ->
			{error, notconnect}
	end.

-spec receive_exec_result(string(), integer(), integer()) ->
		  {ok, string(), integer()} | {error, term()}.
receive_exec_result(Data, ExitStatus, Timeout) ->
	lager:debug("~p", [{Data, ExitStatus}]),
	receive
		{ssh_cm, _, {data, _, _, NewData}} ->
			List = binary:bin_to_list(NewData),
			lager:info("~p", [{data, List}]),
			receive_exec_result(Data ++ List, ExitStatus, Timeout);
		{ssh_cm, _, {eof, _}} ->
			lager:info("~p", [{eof}]),
			receive_exec_result(Data, ExitStatus, Timeout);
		{ssh_cm, _, {exit_status, _, NewExitStatus}} ->
			lager:info("~p", [{exit_status, NewExitStatus}]),
			receive_exec_result(Data, NewExitStatus, Timeout);
		{ssh_cm, _, {closed, _}} ->
			lager:info("~p", [{closed}]),
			{ok, Data, ExitStatus};
		Other ->
			lager:info("~p", [{other, Other}]),
			receive_exec_result(Data, ExitStatus, Timeout)
		after Timeout ->
			case is_integer(ExitStatus) of
				true ->
					{ok, Data, ExitStatus};
				false ->
					{error, timeout}
			end
	end.

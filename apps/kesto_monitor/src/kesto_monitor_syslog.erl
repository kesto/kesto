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
%% @doc syslog監視項用モジュール

-module(kesto_monitor_syslog).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").
-include_lib("kesto_reciever/include/kesto_reciever.hrl").

-export([
		 check/2
		]).

%% @spec check(#monitor_conf{}, #syslog{}) -> match | nomatch | {error, atom()} | {error, Error}
%% @doc 受信データを監視項目と照合する。
check(Conf, Data) when is_record(Conf, monitor_conf) and is_record(Data, syslog) ->
	lager:info("syslog監視項目照合開始 : ~p, ~p", [Conf, Data]),
	{ok, List} = kesto_core_repository:get_facility_id(Data#syslog.host, true),
	{ok, TargetList} = kesto_core_repository:get_node_facility_id_list(Conf#monitor_conf.facility_id, true),
	case check_fasility_list(List, TargetList) of
		{true, FacilityID} ->
			case loop_conf_list(Conf#monitor_conf.conf, Data) of
				{match, SyslogConf} ->
					notify(Conf, SyslogConf, Data, FacilityID),
					match;
				nomatch ->
					nomatch
			end;
		false ->
			nomatch
	end;
check(_, _) ->
	{error, undefined}.

check_fasility_list([], _) ->
	false;
check_fasility_list([FacilityID | T], List) ->
	case lists:member(FacilityID, List) of
		true ->
			{true, FacilityID};
		false ->
			check_fasility_list(T, List)
	end.

loop_conf_list([], _) -> nomatch;
loop_conf_list([Conf | T], Data) when is_record(Conf, syslog_conf) ->
	case check_conf(Conf, Data) of
		match ->
			{match, Conf};
		nomatch ->
			loop_conf_list(T, Data);
		{error, _} ->
			loop_conf_list(T, Data)
	end;
loop_conf_list([_ | T], Data) ->
	loop_conf_list(T, Data).

check_conf(Conf, Data) when is_record(Conf, syslog_conf) and is_record(Data, syslog) ->
	lager:info("syslog監視項目の詳細定義照合開始 : ~p, ~p", [Conf, Data]),
	case is_case_sensitive(Conf) of
		true ->
			Regexp = [string:to_upper(Conf#syslog_conf.pattern)],
			Subject = [string:to_upper(Data#syslog.body)],
			run_regexp(Regexp, Subject);
		false ->
			Regexp = [Conf#syslog_conf.pattern],
			Subject = [Data#syslog.body],
			run_regexp(Regexp, Subject)
	end;
check_conf(_, _) ->
	{error, undefined}.

is_case_sensitive(Conf) when is_record(Conf, syslog_conf) ->
	#syslog_conf{case_sensitive=Case_sensitive} = Conf,
	case Case_sensitive of
		true ->
			true;
		_ ->
			false
	end;
is_case_sensitive(_) ->
	false.

run_regexp(Regexp, Subject) ->
	case re:compile(Regexp) of
		{ok, MP} ->
			lager:debug("reオブジェクトをコンパイルしました。 : ~p, ~p", [Regexp, MP]),
			case re:run(Subject, MP) of
				{match, Captured} ->
					lager:info("ログが正規表現にマッチしました。 : ~p, ~p, ~p", [Regexp, Subject, Captured]),
					match;
				nomatch ->
					lager:info("ログが正規表現にマッチしませんでした。 : ~p, ~p", [Regexp, Subject]),
					nomatch
			end;
		{error, Error} ->
			lager:error("reオブジェクトのコンパイルに失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

notify(Conf, SyslogConf, Data, FacilityID) ->
	case kesto_core_repository:get(FacilityID) of
		{ok, Node} ->
			case SyslogConf#syslog_conf.mode of
				output ->
					case is_list(Conf#monitor_conf.notify_id) of
						true ->
							NotifyInfo = #notify_info{id=Conf#monitor_conf.notify_id, 
													  priority=SyslogConf#syslog_conf.priority, 
													  timestamp=calendar:local_time(), 
													  timestamp_raw=Data#syslog.timestamp, 
													  monitor_type=syslog, 
													  monitor_id=Conf#monitor_conf.id, 
													  facility_id=FacilityID, 
													  facility_name=Node#node.name, 
													  message=SyslogConf#syslog_conf.message, 
													  org_message=Data#syslog.body},
							DocIdx = riak_core_util:chash_key({<<"syslog">>, term_to_binary(now())}),
							PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
							[IdxNode] = PrefList,
							kesto_core_notify_vnode:execute(IdxNode, NotifyInfo);
						false ->
							Event = #event{id=kesto_core_event:get_id(),
										   priority=SyslogConf#syslog_conf.priority, 
										   timestamp=calendar:local_time(), 
										   timestamp_raw=Data#syslog.timestamp, 
										   monitor_type=syslog, 
										   monitor_id=Conf#monitor_conf.id, 
										   facility_id=FacilityID, 
										   facility_name=Node#node.name, 
										   message=SyslogConf#syslog_conf.message, 
										   org_message=Data#syslog.body,
										   check=false, 
										   comment=""},
							kesto_core_event:put(Event)
					end;
				_ ->
					ok
			end;
		{error, _Error} ->
			ok
	end.
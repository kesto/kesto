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
%% @doc kesto_reciever's application module.

-module(kesto_demo).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").
-include_lib("kesto_monitor/include/kesto_monitor.hrl").
-include_lib("kesto_reciever/include/kesto_reciever.hrl").
-include_lib("kesto_job/include/kesto_job.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-compile(export_all).

%% @doc ノード登録
-spec demo1() -> ok.
demo1() ->
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% ノード「test1」を登録
	Node1 = #node{id="test1", 
				  name="test用ノード1", 
				  description="test用ノード1 説明", 
				  type=linux, 
				  enabled=true, 
				  ipv4=["192.168.0.1"], 
				  ipv6=[], 
				  ssh_port=22, 
				  ssh_user="kesto_test", 
				  ssh_password="kesto_test", 
				  hostname=["Takahiros-MBA"], 
				  create_timestamp={{2012,6,5},{11,30,14}},
				  update_timestamp={{2012,6,5},{11,33,14}}},
	ok = kesto_core_repository_vnode:put(IdxNode, Node1),
	
	% ノード「test2」を登録
	Node2 = #node{id="test2", 
				  name="test用ノード2", 
				  description="test用ノード2 説明", 
				  type=linux, 
				  enabled=true, 
				  ipv4=["192.168.0.2"], 
				  ipv6=[], 
				  ssh_port=22, 
				  ssh_user="kesto_test", 
				  ssh_password="kesto_test", 
				  hostname=["shidara-pc"], 
				  create_timestamp={{2012,6,5},{11,30,14}},
				  update_timestamp={{2012,6,5},{11,33,14}}},
	ok = kesto_core_repository_vnode:put(IdxNode, Node2).

%% @doc スコープ登録
-spec demo2() -> ok.
demo2() ->
	DocIdx = riak_core_util:chash_key({<<"scope">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% スコープ「scope1」を登録
	Scope1 = #scope{id="scope1", 
					name="test用スコープ1", 
					description="test用スコープ1 説明", 
					enabled=true, 
					children=[],
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	ok = kesto_core_repository_vnode:put(IdxNode, Scope1).

%% @doc スコープへのノード割り当て
-spec demo3() -> ok.
demo3() ->
	DocIdx = riak_core_util:chash_key({<<"scope">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% スコープ「scope1」へノード「test1」と「test2」を割り当てる
	ok = kesto_core_repository_vnode:assign(IdxNode, "scope1", ["test1", "test2"]).

%% @doc メール通知登録
-spec demo4() -> ok.
demo4() ->
	DocIdx = riak_core_util:chash_key({<<"notify_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% メールテンプレート「template1」を登録
	% subject：Kesto Alarm
	% body：アラームを検知しました。
	%       重要度：$priority
	%       監視項目ID：$monitor_id
	%       ファシリティID：$facility_id
	%       ファシリティ名：$facility_name
	%       受信日時：$timestamp
	%       出力日時：$timestamp_raw
	%       メッセージ：$message
	%       オリジナルメッセージ：$org_message
	Template1 = #mail_template_conf{id="template1",
									description="test1 description", 
									subject="Kesto Alarm",
									body="アラームを検知しました。\r\n\r\n重要度：$priority\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	ok = kesto_core_mail_vnode:put(IdxNode, Template1),

	% メール通知「notify_syslog_mail」を登録
	Conf1 = #notify_conf{id="notify_syslog_mail",
						 description="syslog用メール通知定義", 
						 type=mail,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_mail_conf{mail_template_id="template1",
												info_enabled=true, 
												info_address=["shidara@conversion.co.jp"], 
												warning_enabled=true, 
												warning_address=["shidara@conversion.co.jp"], 
												error_enabled=true, 
												error_address=["shidara@conversion.co.jp"], 
												unknown_enabled=true, 
												unknown_address=["shidara@conversion.co.jp"]}},
	ok = kesto_core_notify_vnode:put(IdxNode, Conf1),

	% メール通知「notify_ping_mail」を登録
	Conf2 = #notify_conf{id="notify_ping_mail",
						 description="ping用メール通知定義", 
						 type=mail,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_mail_conf{mail_template_id="template1",
												info_enabled=true, 
												info_address=["shidara@conversion.co.jp"], 
												warning_enabled=true, 
												warning_address=["shidara@conversion.co.jp"], 
												error_enabled=true, 
												error_address=["shidara@conversion.co.jp"], 
												unknown_enabled=true, 
												unknown_address=["shidara@conversion.co.jp"]}},
	ok = kesto_core_notify_vnode:put(IdxNode, Conf2).

%% @doc イベント通知登録
-spec demo5() -> ok.
demo5() ->
	DocIdx = riak_core_util:chash_key({<<"notify_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% イベント通知「notify_syslog_event」を登録
	Conf1 = #notify_conf{id="notify_syslog_event",
						 description="syslog用イベント通知定義", 
						 type=event,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_event_conf{info_enabled=true, 
												 info_check=true, 
												 warning_enabled=true, 
												 warning_check=true, 
												 error_enabled=true, 
												 error_check=true, 
												 unknown_enabled=true, 
												 unknown_check=true}},
	ok = kesto_core_notify_vnode:put(IdxNode, Conf1),

	% イベント通知「notify_ping_event」を登録
	Conf2 = #notify_conf{id="notify_ping_event",
						 description="ping用イベント通知定義", 
						 type=event,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_event_conf{info_enabled=true, 
												 info_check=true, 
												 warning_enabled=true, 
												 warning_check=true, 
												 error_enabled=true, 
												 error_check=true, 
												 unknown_enabled=true, 
												 unknown_check=true}},
	ok = kesto_core_notify_vnode:put(IdxNode, Conf2).

%% @doc syslog監視項目登録
-spec demo6() -> ok.
demo6() ->
	DocIdx = riak_core_util:chash_key({<<"monitor_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_monitor),
	[IdxNode] = PrefList,
	
	% syslog監視項目「conf1」を登録
	Conf1 = #monitor_conf{id="conf1", 
						  name="syslog監視項目", 
						  description="syslog監視項目 説明", 
						  type=syslog, 
						  facility_id="scope1", 
						  calendar_id="", 
						  notify_id=["notify_syslog_event", "notify_syslog_mail"],
						  enabled=true, 
						  conf=[#syslog_conf{order_no=1,
											 case_sensitive=false,
											 pattern=".*test.*",
											 mode=output,
											 priority=info,
											 message="ログを検知しました。"},
								#syslog_conf{order_no=2,
											 case_sensitive=false,
											 pattern=".*error.*",
											 mode=output,
											 priority=error,
											 message="ログを検知しました。"}]},
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf1).

%% @doc ping監視項目登録
-spec demo7() -> ok.
demo7() ->
	DocIdx = riak_core_util:chash_key({<<"monitor_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_monitor),
	[IdxNode] = PrefList,
	
	% ping監視項目「conf2」を登録
	Conf1 = #monitor_conf{id="conf2", 
						  name="ping監視項目", 
						  description="ping監視項目 説明", 
						  type=ping, 
						  facility_id="scope1", 
						  calendar_id="", 
						  cycle=60, 
						  times=2, 
						  interval=1, 
						  timeout=5000,
						  enabled=true,
						  notify_id=["notify_ping_event", "notify_ping_mail"],
						  conf=#ping_conf{info_low=1000.0,
										   info_high=1.0,
										   warn_low=3000.0,
										   warn_high=51.0}},
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf1).

%% @doc 通知抑制条件変更
-spec demo8() -> ok.
demo8() ->
	DocIdx = riak_core_util:chash_key({<<"notify_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	% メール通知「notify_syslog_mail」の通知抑制を変更
	Conf1 = kesto_core_notify_vnode:get(IdxNode, "notify_syslog_mail"),
	NewConf1 = Conf1#notify_conf{initial_count=1,
								 renotify_type=count,
								 renotify_value=3},
	ok = kesto_core_notify_vnode:update(IdxNode, NewConf1),
	
	% イベント通知「notify_syslog_event」の通知抑制を変更
	Conf2 = kesto_core_notify_vnode:get(IdxNode, "notify_syslog_event"),
	NewConf2 = Conf2#notify_conf{initial_count=1,
								 renotify_type=count,
								 renotify_value=3},
	ok = kesto_core_notify_vnode:update(IdxNode, NewConf2),
	
	% メール通知「notify_ping_mail」の通知抑制を変更
	Conf3 = kesto_core_notify_vnode:get(IdxNode, "notify_ping_mail"),
	NewConf3 = Conf3#notify_conf{initial_count=1,
								 renotify_type=ignore},
	ok = kesto_core_notify_vnode:update(IdxNode, NewConf3),
	
	% イベント通知「notify_ping_event」の通知抑制を変更
	Conf4 = kesto_core_notify_vnode:get(IdxNode, "notify_ping_event"),
	NewConf4 = Conf4#notify_conf{initial_count=1,
								 renotify_type=ignore},
	ok = kesto_core_notify_vnode:update(IdxNode, NewConf4).

%% @doc デモ用ジョブネット登録(デモ1)
-spec job1() -> ok.
job1() ->
	Preflist = kesto_job_session_util:get_preflist(),

	Net1 = #job_conf{id="demo1", 
					 name="デモ１", 
					 description="", 
					 type=net, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="root", 
					 child_id=[{"group1", "net_a"}, {"group1", "job_a"}, {"group1", "job_e"}], 
					 previous_id=[], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false}},
	ok = kesto_job_vnode:put_conf(Preflist, Net1),
	
	NetA = #job_conf{id="net_a", 
					 name="ジョブネットA", 
					 description="", 
					 type=net, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="net1",
					 child_id=[{"group1", "job_b"}, {"group1", "job_c"}, {"group1", "job_d"}], 
					 previous_id=[{"group1", "job_a"}], 
					 next_id=[{"group1", "job_e"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false}},
	ok = kesto_job_vnode:put_conf(Preflist, NetA),
	
	JobA = #job_conf{id="job_a", 
					 name="ジョブA", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="demo1",
					 child_id=[], 
					 previous_id=[], 
					 next_id=[{"group1", "net_a"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobA),
	
	JobB = #job_conf{id="job_b", 
					 name="ジョブB", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="net_a",
					 child_id=[], 
					 previous_id=[], 
					 next_id=[{"group1", "job_c"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobB),
	
	JobC = #job_conf{id="job_c", 
					 name="ジョブC", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="net_a",
					 child_id=[], 
					 previous_id=[{"group1", "job_b"}], 
					 next_id=[{"group1", "job_d"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobC),
	
	JobD = #job_conf{id="job_d", 
					 name="ジョブD", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="net_a",
					 child_id=[], 
					 previous_id=[{"group1", "job_c"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobD),
	
	JobE = #job_conf{id="job_e", 
					 name="ジョブE", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group1", 
					 parent_id="demo1",
					 child_id=[], 
					 previous_id=[{"group1", "net_a"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobE).
	
%% @doc デモ用ジョブネット実行(デモ1)
-spec run1() -> ok.
run1() ->
	{ok, SessionID} = kesto_job_vnode:run(kesto_job_session_util:get_preflist(), "group1", "demo1").

%% @doc デモ用ジョブネット登録(デモ2)
-spec job2() -> ok.
job2() ->
	Preflist = kesto_job_session_util:get_preflist(),

	Net1 = #job_conf{id="demo2", 
					 name="デモ２", 
					 description="", 
					 type=net, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="root", 
					 child_id=[{"group2", "job_a"}, 
							   {"group2", "job_b"}, 
							   {"group2", "job_c"}, 
							   {"group2", "job_d"}, 
							   {"group2", "job_e"}, 
							   {"group2", "job_f"}, 
							   {"group2", "job_g"}], 
					 previous_id=[], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false}},
	ok = kesto_job_vnode:put_conf(Preflist, Net1),
	
	JobA = #job_conf{id="job_a", 
					 name="ジョブA", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[], 
					 next_id=[{"group2", "job_b"}, {"group2", "job_d"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobA),
	
	JobB = #job_conf{id="job_b", 
					 name="ジョブB", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_a"}], 
					 next_id=[{"group2", "job_c"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobB),
	
	JobC = #job_conf{id="job_c", 
					 name="ジョブC", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_b"}], 
					 next_id=[{"group2", "job_e"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobC),
	
	JobD = #job_conf{id="job_d", 
					 name="ジョブD", 
					 description="", 
					 type=job, 
					 recovery=true, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_a"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobD),
	
	JobE = #job_conf{id="job_e", 
					 name="ジョブE", 
					 description="", 
					 type=judgment, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_c"}], 
					 dependent_id={"group2", "job_g"}, 
					 next_id=[{"group2", "job_f"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 judgement=#job_conf_judgement{type=end_state,
												   value=error}},
	ok = kesto_job_vnode:put_conf(Preflist, JobE),
	
	JobF = #job_conf{id="job_f", 
					 name="ジョブF", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_e"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobF),
	
	JobG = #job_conf{id="job_g", 
					 name="ジョブG", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_e"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/true", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobG).

%% @doc デモ用ジョブネット変更(デモ2)
%%       リカバリジョブの実行
-spec job3() -> ok.
job3() ->
	Preflist = kesto_job_session_util:get_preflist(),

	JobA = #job_conf{id="job_a", 
					 name="ジョブA", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[], 
					 next_id=[{"group2", "job_b"}, {"group2", "job_d"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/false", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobA).

%% @doc デモ用ジョブネット変更(デモ2)
%%       判定ジョブの実行
-spec job4() -> ok.
job4() ->
	Preflist = kesto_job_session_util:get_preflist(),
	
	JobC = #job_conf{id="job_c", 
					 name="ジョブC", 
					 description="", 
					 type=job, 
					 recovery=false, 
					 group_id="group2", 
					 parent_id="demo2",
					 child_id=[], 
					 previous_id=[{"group2", "job_b"}], 
					 next_id=[{"group2", "job_e"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=0, 
					 warning_end_value_to=0,
					 control=#job_conf_control{calendar=false, 
											   control_skip=false, 
											   control_reserve=false, 
											   unmatch=false},
					 command=#job_conf_command{cmd_facility_id="test1", 
											   cmd_start_command="/bin/sleep 10;/bin/false", 
											   cmd_stop_command="/bin/true",  
											   cmd_effective_user="root", 
											   cmd_error_end_flg=false}},
	ok = kesto_job_vnode:put_conf(Preflist, JobC).
	
%% @doc デモ用ジョブネット実行(デモ2)
-spec run2() -> ok.
run2() ->
	{ok, SessionID} = kesto_job_vnode:run(kesto_job_session_util:get_preflist(), "group2", "demo2").

%% @spec list() -> ok.
%% @doc イベント一覧表示
-spec list() -> ok.
list() ->
	DocIdx = riak_core_util:chash_key({<<"event">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	
	List = kesto_core_event_vnode:get_list(IdxNode),
	lager:info("---------------------------------------------------------"),
	print_event_list(List),
	lager:info("---------------------------------------------------------").

print_event_list([]) ->
	ok;
print_event_list([Event | T]) ->
	{Id, 
	 Priority, 
	 Timestamp, 
	 Timestamp_raw, 
	 Monitor_type, 
	 Monitor_id, 
	 Facility_id, 
	 Facility_name, 
	 Message, 
	 Org_message,
	 Check, 
	 Comment} = {Event#event.id,
				 Event#event.priority, 
				 Event#event.timestamp, 
				 Event#event.timestamp_raw, 
				 Event#event.monitor_type, 
				 Event#event.monitor_id, 
				 Event#event.facility_id, 
				 Event#event.facility_name, 
				 Event#event.message, 
				 Event#event.org_message,
				 Event#event.check, 
				 Event#event.comment},
	lager:info("~s, ~p, ~p, ~p, ~p, ~s, ~s, ~s, ~s, ~s, ~p, ~s", [Id, 
																  Priority, 
																  Timestamp, 
																  Timestamp_raw, 
																  Monitor_type, 
																  Monitor_id, 
																  Facility_id, 
																  Facility_name, 
																  Message, 
																  Org_message,
																  Check, 
																  Comment]),
	print_event_list(T).

%% @spec job_list() -> ok.
%% @doc ジョブセッション一覧表示
-spec job_list(session_id) -> ok.
job_list(SessionID) ->
	{ok, List} = kesto_job_session_info:get_list(SessionID, all, all),
	SortedList = lists:sort(fun(X, Y) ->
									kesto_demo:get_id(X) =< kesto_demo:get_id(Y)
							end,
							List),
	lager:info("---------------------------------------------------------"),
	print_job_list(SortedList),
	lager:info("---------------------------------------------------------").

-spec get_id(job_session_info()) -> string().
get_id(Session) when is_record(Session, job_session) ->
	Session#job_session.id;
get_id(SessionJob) when is_record(SessionJob, job_session_job) ->
	lists:concat([SessionJob#job_session_job.id, 
				  "_", 
				  SessionJob#job_session_job.group_id,
				  "_", 
				  SessionJob#job_session_job.job_id]);
get_id(SessionNode) when is_record(SessionNode, job_session_node) ->
	lists:concat([SessionNode#job_session_node.id, 
				  "_", 
				  SessionNode#job_session_node.group_id,
				  "_", 
				  SessionNode#job_session_node.job_id, 
				  "_", 
				  SessionNode#job_session_node.facility#node.id]).

print_job_list([]) ->
	ok;
print_job_list([SessionInfo | T]) ->
	print_job_session(SessionInfo),
	print_job_list(T).

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
	lager:info("job_session     : ~s, ~s, ~s, ~p, ~p, ~p", [SessionID,
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
	lager:info("job_session_job  : ~s, ~s, ~s, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p", [SessionID,
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

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

-module(notify_test).

-include("kesto_test.hrl").
-include("kesto_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-ifdef(KESTO_CORE_TEST).
%% -define(RUN_TEST, true).
-endif.

setup() ->
	kesto_core_test_util:common_setup(?MODULE, fun configure/1).

cleanup() ->
	kesto_core_test_util:common_cleanup(?MODULE, fun configure/1).

configure(load) ->
	KVSettings = [{storage_backend, riak_kv_memory_backend},
				  {test, true},
				  {vnode_vclocks, true},
				  {pb_ip, "0.0.0.0"},
				  {pb_port, 0}, % arbitrary #
				  {map_js_vm_count, 4},
				  {reduce_js_vm_count, 3}],
	CoreSettings = [{handoff_ip, "0.0.0.0"},
					{handoff_port, 0},
					{ring_creation_size, 16}],
	KestoCoreSettings = [{put_option, [{w, 2}, {dw, 1}, return_body]},
						 {smtp, [{relay, "test@gmail.com"}, 
								 {username, "test"}, 
								 {password, ""}, 
								 {port, 465}, 
								 {ssl, true}]},
						 {mail_from, "test@gmail.com"},
						 {ssh_client_connect_timeout, 5000}],
	[ application:set_env(riak_core, K, V) || {K,V} <- CoreSettings ],
	[ application:set_env(riak_kv, K, V) || {K,V} <- KVSettings ],
	[ application:set_env(kesto_core, K, V) || {K,V} <- KestoCoreSettings ],
	ok;
configure(start) ->
	riak_core:wait_for_service(riak_kv),
	riak_core:wait_for_service(kesto_core);
configure(_) ->
	ok.

-ifdef(RUN_TEST).
notify_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun notify_conf/0,
	  fun notify_conf_error/0,
	  fun notify_conf_get_list/0,
	  fun mail_template_conf/0,
	  fun mail_template_conf_error/0,
	  fun mail_template_conf_get_list/0,
	  fun execute_1_all/0,
	  fun execute_1_ignore/0,
	  fun execute_1_period/0,
	  fun execute_1_count/0,
	  fun execute_3_all/0,
	  fun execute_3_ignore/0,
	  fun execute_3_period/0,
	  fun execute_3_count/0,
	  fun execute_1_ignore_priority_change/0,
	  fun execute_3_ignore_priority_change/0,
	  fun notify_mail/0,
	  fun notify_event/0,
	  {timeout, 30, [fun notify_command/0]}
	 ]
	}.
-endif.

notify_conf() ->
	Conf = #notify_conf{id="test",
						description="test description", 
						type=event,
						initial_count=1,
						renotify_type=ignore,
						enabled=true, 
						conf=""},
	?debugVal(Conf),
	
	DocIdx = riak_core_util:chash_key({<<"notify">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_notify_vnode:put(IdxNode, Conf),
	
	Obj1 = kesto_core_notify_vnode:get(IdxNode, Conf#notify_conf.id),
	?debugVal(Obj1),
	?assertMatch(Conf, Obj1),
	
	NewConf = Conf#notify_conf{enabled=false},
	?debugVal(NewConf),
	ok = kesto_core_notify_vnode:update(IdxNode, NewConf),
	
	Obj2 = kesto_core_notify_vnode:get(IdxNode, NewConf#notify_conf.id),
	?debugVal(Obj2),
	?assertMatch(NewConf, Obj2),
	
	ok = kesto_core_notify_vnode:delete(IdxNode, NewConf#notify_conf.id),
	
	{error, notfound} = kesto_core_notify_vnode:get(IdxNode, NewConf#notify_conf.id).

notify_conf_error() ->
	Conf = #notify_conf{id="test",
						description="test description", 
						type=event,
						initial_count=1,
						renotify_type=ignore,
						enabled=true, 
						conf=""},
	?debugVal(Conf),
	
	{error, notfound} = kesto_core_notify:get(Conf#notify_conf.id),
	{error, notfound} = kesto_core_notify:update(Conf),
	{error, notfound} = kesto_core_notify:delete(Conf#notify_conf.id),
	
	DocIdx = riak_core_util:chash_key({<<"notify">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_notify_vnode:get(IdxNode, Conf#notify_conf.id),
	{error, notfound} = kesto_core_notify_vnode:update(IdxNode, Conf),
	{error, notfound} = kesto_core_notify_vnode:delete(IdxNode, Conf#notify_conf.id).

notify_conf_get_list() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=event,
						 initial_count=1,
						 renotify_type=ignore,
						 enabled=true, 
						 conf=""},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Conf2 = #notify_conf{id="test2",
						 description="test2 description", 
						 type=event,
						 initial_count=1,
						 renotify_type=ignore,
						 enabled=false, 
						 conf=""},
	?debugVal(Conf2),
	ok = kesto_core_notify:put(Conf2),
	
	DocIdx = riak_core_util:chash_key({<<"notify">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	Obj1 = kesto_core_notify_vnode:get_list(IdxNode, true),
	?debugVal(Obj1),
	?assert(lists:member(Conf1, Obj1)),
	?assertNot(lists:member(Conf2, Obj1)),
	
	Obj2 = kesto_core_notify_vnode:get_list(IdxNode, false),
	?debugVal(Obj2),
	?assertNot(lists:member(Conf1, Obj2)),
	?assert(lists:member(Conf2, Obj2)),
	
	Obj3 = kesto_core_notify_vnode:get_list(IdxNode, all),
	?debugVal(Obj3),
	?assert(lists:member(Conf1, Obj3)),
	?assert(lists:member(Conf2, Obj3)).

mail_template_conf() ->
	Conf = #mail_template_conf{id="test1",
							   description="test1 description", 
							   subject="[$facility_id] Kesto Alarm",
							   body="アラームを検知しました。\r\n\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	?debugVal(Conf),
	
	DocIdx = riak_core_util:chash_key({<<"mail_template">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_mail_vnode:put(IdxNode, Conf),
	
	Obj1 = kesto_core_mail_vnode:get(IdxNode, Conf#mail_template_conf.id),
	?debugVal(Obj1),
	?assertMatch(Conf, Obj1),
	
	NewConf = Conf#mail_template_conf{description="test1 description test"},
	?debugVal(NewConf),
	ok = kesto_core_mail_vnode:update(IdxNode, NewConf),
	
	Obj2 = kesto_core_mail_vnode:get(IdxNode, NewConf#mail_template_conf.id),
	?debugVal(Obj2),
	?assertMatch(NewConf, Obj2),
	
	ok = kesto_core_mail_vnode:delete(IdxNode, NewConf#mail_template_conf.id),
	
	{error, notfound} = kesto_core_mail_vnode:get(IdxNode, NewConf#mail_template_conf.id).

mail_template_conf_error() ->
	Conf = #mail_template_conf{id="test1",
							   description="test1 description", 
							   subject="[$facility_id] Kesto Alarm",
							   body="アラームを検知しました。\r\n\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	?debugVal(Conf),
	
	{error, notfound} = kesto_core_mail_template:get(Conf#mail_template_conf.id),
	{error, notfound} = kesto_core_mail_template:update(Conf),
	{error, notfound} = kesto_core_mail_template:delete(Conf#mail_template_conf.id),
	
	DocIdx = riak_core_util:chash_key({<<"mail_template">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_mail_vnode:get(IdxNode, Conf#mail_template_conf.id),
	{error, notfound} = kesto_core_mail_vnode:update(IdxNode, Conf),
	{error, notfound} = kesto_core_mail_vnode:delete(IdxNode, Conf#mail_template_conf.id).

mail_template_conf_get_list() ->
	Conf1 = #mail_template_conf{id="test1",
								description="test1 description", 
								subject="[$facility_id] Kesto Alarm",
								body="アラームを検知しました。\r\n\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	?debugVal(Conf1),
	ok = kesto_core_mail_template:put(Conf1),
	
	Conf2 = #mail_template_conf{id="test2",
								description="test2 description", 
								subject="[$facility_id] Kesto Alarm",
								body="アラームを検知しました。\r\n\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	?debugVal(Conf2),
	ok = kesto_core_mail_template:put(Conf2),
	
	DocIdx = riak_core_util:chash_key({<<"mail_template">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	Obj1 = kesto_core_mail_vnode:get_list(IdxNode),
	?debugVal(Obj1),
	?assert(lists:member(Conf1, Obj1)),
	?assert(lists:member(Conf2, Obj1)).

execute_1_all() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assert(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assert(lists:member({"test1", Info2}, List2)),
	ok.

execute_1_ignore() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=1,
						 renotify_type=ignore,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assert(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assertNot(lists:member({"test1", Info4}, List4)),
	
	ok.

execute_1_period() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=1,
						 renotify_type=period,
						 renotify_value=60,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assert(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,13}}, 
						 timestamp_raw={{2012,6,5},{11,31,09}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,13}}, 
						 timestamp_raw={{2012,6,5},{11,32,09}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assertNot(lists:member({"test1", Info4}, List4)),
	
	Info5 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info5),
	{ok, List5} = kesto_core_notify_executer:execute(Info5),
	?assert(lists:member({"test1", Info5}, List5)),
	
	ok.

execute_1_count() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=1,
						 renotify_type=count,
						 renotify_value=3,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assert(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assertNot(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assert(lists:member({"test1", Info4}, List4)),
	
	Info5 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,34,14}}, 
						 timestamp_raw={{2012,6,5},{11,34,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info5),
	{ok, List5} = kesto_core_notify_executer:execute(Info5),
	?assertNot(lists:member({"test1", Info5}, List5)),
	
	Info6 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,35,14}}, 
						 timestamp_raw={{2012,6,5},{11,35,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info6),
	{ok, List6} = kesto_core_notify_executer:execute(Info6),
	?assertNot(lists:member({"test1", Info6}, List6)),
	
	Info7 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,36,14}}, 
						 timestamp_raw={{2012,6,5},{11,36,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info7),
	{ok, List7} = kesto_core_notify_executer:execute(Info7),
	?assert(lists:member({"test1", Info7}, List7)),
	
	ok.

execute_3_all() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=3,
						 renotify_type=all,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assertNot(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assert(lists:member({"test1", Info4}, List4)),
	
	ok.

execute_3_ignore() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=3,
						 renotify_type=ignore,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assertNot(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assertNot(lists:member({"test1", Info4}, List4)),
	
	ok.

execute_3_period() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=3,
						 renotify_type=period,
						 renotify_value=60,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assertNot(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,13}}, 
						 timestamp_raw={{2012,6,5},{11,33,09}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assertNot(lists:member({"test1", Info4}, List4)),
	
	Info5 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info5),
	{ok, List5} = kesto_core_notify_executer:execute(Info5),
	?assert(lists:member({"test1", Info5}, List5)),
	
	Info6 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,34,13}}, 
						 timestamp_raw={{2012,6,5},{11,34,09}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List6} = kesto_core_notify_executer:execute(Info6),
	?assertNot(lists:member({"test1", Info6}, List6)),
	
	Info7 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,34,14}}, 
						 timestamp_raw={{2012,6,5},{11,34,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info7),
	{ok, List7} = kesto_core_notify_executer:execute(Info7),
	?assert(lists:member({"test1", Info7}, List7)),
	
	ok.

execute_3_count() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=3,
						 renotify_type=count,
						 renotify_value=3,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assertNot(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assert(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assertNot(lists:member({"test1", Info4}, List4)),
	
	Info5 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,34,14}}, 
						 timestamp_raw={{2012,6,5},{11,34,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info5),
	{ok, List5} = kesto_core_notify_executer:execute(Info5),
	?assertNot(lists:member({"test1", Info5}, List5)),
	
	Info6 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,35,14}}, 
						 timestamp_raw={{2012,6,5},{11,35,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info6),
	{ok, List6} = kesto_core_notify_executer:execute(Info6),
	?assert(lists:member({"test1", Info6}, List6)),
	
	Info7 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,36,14}}, 
						 timestamp_raw={{2012,6,5},{11,36,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info7),
	{ok, List7} = kesto_core_notify_executer:execute(Info7),
	?assertNot(lists:member({"test1", Info7}, List7)),
	
	Info8 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,37,14}}, 
						 timestamp_raw={{2012,6,5},{11,37,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info8),
	{ok, List8} = kesto_core_notify_executer:execute(Info8),
	?assertNot(lists:member({"test1", Info8}, List8)),
	
	Info9 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,38,14}}, 
						 timestamp_raw={{2012,6,5},{11,38,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info9),
	{ok, List9} = kesto_core_notify_executer:execute(Info9),
	?assert(lists:member({"test1", Info9}, List9)),
	
	ok.

execute_1_ignore_priority_change() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=1,
						 renotify_type=ignore,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assert(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assert(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assertNot(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assert(lists:member({"test1", Info4}, List4)),
	
	ok.

execute_3_ignore_priority_change() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=test,
						 initial_count=3,
						 renotify_type=ignore,
						 enabled=true},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	{ok, List1} = kesto_core_notify_executer:execute(Info1),
	?assertNot(lists:member({"test1", Info1}, List1)),
	
	Info2 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,31,14}}, 
						 timestamp_raw={{2012,6,5},{11,31,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info2),
	{ok, List2} = kesto_core_notify_executer:execute(Info2),
	?assertNot(lists:member({"test1", Info2}, List2)),
	
	Info3 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,32,14}}, 
						 timestamp_raw={{2012,6,5},{11,32,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info3),
	{ok, List3} = kesto_core_notify_executer:execute(Info3),
	?assertNot(lists:member({"test1", Info3}, List3)),
	
	Info4 = #notify_info{id=["test1"], 
						 priority=info, 
						 timestamp={{2012,6,5},{11,33,14}}, 
						 timestamp_raw={{2012,6,5},{11,33,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info4),
	{ok, List4} = kesto_core_notify_executer:execute(Info4),
	?assert(lists:member({"test1", Info4}, List4)),
	
	ok.

notify_mail() ->
	Template1 = #mail_template_conf{id="template1",
									description="test1 description", 
									subject="[$facility_id] Kesto Alarm [$facility_id]",
									body="アラームを検知しました。\r\n\r\n重要度：$priority\r\n監視項目ID：$monitor_id\r\nファシリティID：$facility_id\r\nファシリティ名：$facility_name\r\n受信日時：$timestamp\r\n出力日時：$timestamp_raw\r\nメッセージ：$message\r\nオリジナルメッセージ：$org_message"},
	?debugVal(Template1),
	ok = kesto_core_mail_template:put(Template1),
	
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=mail,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_mail_conf{mail_template_id="template1",
												info_enabled=true, 
												info_address=["test@gmail.com"], 
												warning_enabled=true, 
												warning_address=["test@gmail.com"], 
												error_enabled=true, 
												error_address=["test@gmail.com"], 
												unknown_enabled=true, 
												unknown_address=["test@gmail.com"]}},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	ok = kesto_core_mail:notify("test1", Info1),
	ok.

notify_event() ->
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
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
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	ok = kesto_core_event:notify("test1", Info1),
	ok.

notify_command() ->
	Node = #node{id="node1", 
				 name="node1 name", 
				 description="node1 description", 
				 type=linux, 
				 enabled=true, 
				 ipv4=["127.0.0.1"], 
				 ipv6=["::1"], 
				 ssh_port=22, 
				 hostname=["testhost1"], 
				 create_timestamp={{2012,6,5},{11,30,14}},
				 update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Node),
	ok = kesto_core_repository:put(Node),
	
	Conf1 = #notify_conf{id="test1",
						 description="test1 description", 
						 type=event,
						 initial_count=1,
						 renotify_type=all,
						 enabled=true,
						 conf=#notify_command_conf{info_enabled=true, 
												   info_user="kesto_test", 
												   info_password="kesto_test", 
												   info_cmd="logger $org_message", 
												   info_cmd_timeout=5000, 
												   warning_enabled=true, 
												   warning_user="kesto_test", 
												   warning_password="kesto_test", 
												   warning_cmd="logger $org_message", 
												   warning_cmd_timeout=5000, 
												   error_enabled=true, 
												   error_user="kesto_test", 
												   error_password="kesto_test", 
												   error_cmd="logger $org_message", 
												   error_cmd_timeout=5000, 
												   unknown_enabled=true, 
												   unknown_user="kesto_test", 
												   unknown_password="kesto_test", 
												   unknown_cmd="logger $org_message", 
												   unknown_cmd_timeout=5000}},
	?debugVal(Conf1),
	ok = kesto_core_notify:put(Conf1),
	
	Info1 = #notify_info{id=["test1"], 
						 priority=error, 
						 timestamp={{2012,6,5},{11,30,14}}, 
						 timestamp_raw={{2012,6,5},{11,30,10}}, 
						 monitor_type=syslog, 
						 monitor_id="syslog1", 
						 facility_id="node1", 
						 facility_name="node1 name", 
						 message="message", 
						 org_message="original message"},
	?debugVal(Info1),
	ok = kesto_core_command:notify("test1", Info1),
	ok.

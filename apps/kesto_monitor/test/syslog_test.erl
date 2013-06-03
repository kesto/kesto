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

-module(syslog_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("kesto_core/include/kesto_test.hrl").
-include("kesto_monitor.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").
-include_lib("kesto_reciever/include/kesto_reciever.hrl").
-include_lib("kesto_scheduler/include/kesto_scheduler.hrl").

-compile(export_all).

-ifdef(KESTO_MONITOR_TEST).
-define(RUN_TEST, true).
-endif.

setup() ->
	kesto_monitor_test_util:common_setup(?MODULE, fun configure/1).

cleanup() ->
	kesto_monitor_test_util:common_cleanup(?MODULE, fun configure/1).

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
						 {smtp, []},
						 {mail_from, ""},
						 {ssh_client_connect_timeout, 5000}],
	KestoRecieverSettings = [{syslog_port, 10514}],
	KestoMonitorSettings = [{fping_path, "sudo /usr/local/sbin/fping"},
							{fping6_path, "sudo /usr/local/sbin/fping6"}],
	KestoPollerSettings = [],
	KestoSchedulerSettings = [{check_interval, 60},
							  {check_cycle, 1},
							  {check_offset, 30}],

	[ application:set_env(riak_core, K, V) || {K,V} <- CoreSettings ],
	[ application:set_env(riak_kv, K, V) || {K,V} <- KVSettings ],
	[ application:set_env(kesto_core, K, V) || {K,V} <- KestoCoreSettings ],
	[ application:set_env(kesto_reciever, K, V) || {K,V} <- KestoRecieverSettings ],
	[ application:set_env(kesto_monitor, K, V) || {K,V} <- KestoMonitorSettings ],
	[ application:set_env(kesto_poller, K, V) || {K,V} <- KestoPollerSettings ],
	[ application:set_env(kesto_scheduler, K, V) || {K,V} <- KestoSchedulerSettings ],
	ok;
configure(start) ->
	riak_core:wait_for_service(riak_kv),
	riak_core:wait_for_service(kesto_core),
	riak_core:wait_for_service(kesto_reciever),
	riak_core:wait_for_service(kesto_monitor),
	riak_core:wait_for_service(kesto_poller),
	riak_core:wait_for_service(kesto_scheduler);
configure(_) ->
	ok.

-ifdef(RUN_TEST).
syslog_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun monitor_syslog/0
	 ]
	}.
-endif.

monitor_syslog() ->
	Node1 = #node{id="node1", 
				  name="node1 name", 
				  description="node1 description", 
				  type=linux, 
				  enabled=true, 
				  ipv4=["127.0.0.1"], 
				  ipv6=[], 
				  hostname=["testhost1"], 
				  create_timestamp={{2012,6,5},{11,30,14}},
				  update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Node1),
	
	ok = kesto_core_repository:put(Node1),
	
	Node2 = Node1#node{id="node2", 
					   name="node2 name", 
					   description="node2 description",
					   enabled=false,
					   hostname=["testhost2"]},
	?debugVal(Node2),
	ok = kesto_core_repository:put(Node2),
	
	Scope1 = #scope{id="scope1", 
					name="scope1 name", 
					description="scope1 description", 
					enabled=true, 
					children=["node1", "node2"], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	ok = kesto_core_repository:put(Scope1),
	
	Conf1 = #monitor_conf{id="conf1", 
						  name="syslog conf 1", 
						  description="syslog conf 1 description", 
						  type=syslog, 
						  facility_id="scope1", 
						  calendar_id="", 
						  enabled=true, 
						  notify_id=test,
						  conf=[#syslog_conf{order_no=1,
											 case_sensitive=false,
											 pattern=".*test.*",
											 mode=output,
											 priority=error,
											 message="test"}]},
	?debugVal(Conf1),
	
	%% 	Conf2 = #monitor_confid="conf2", 
	%% 						 name="syslog conf 2", 
	%% 						 description="syslog conf 2 description", 
	%% 						 type=syslog, 
	%% 						 facility_id="node2", 
	%% 						 calendar_id="", 
	%% 						 enabled=true, 
	%% 						 conf=[#syslog_conf{order_no=1,
	%% 											case_sensitive=false,
	%% 											pattern=".*test.*",
	%% 											mode=output,
	%% 											priority=error,
	%% 											message="test"}]},
	%% 	?debugVal(Conf2),
	
	DocIdx = riak_core_util:chash_key({<<"monitor_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_monitor),
	[IdxNode] = PrefList,
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf1),
	%% 	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf2),
	
	Data1 = #syslog{priority=158, 
					timestamp={{2012,6,5},{11,30,14}}, 
					host="testhost1", 
					tag="takahiro[2190]", 
					body="test\n"},
	
	ok = kesto_monitor_vnode:check(IdxNode, Data1),
	
	Data2 = Data1#syslog{body="TEST\n"},
	
	ok = kesto_monitor_vnode:check(IdxNode, Data2).

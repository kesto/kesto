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

-module(monitor_test).

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
	riak_core:wait_for_service(kesto_monitor);
configure(_) ->
	ok.

-ifdef(RUN_TEST).
monitor_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun monitor_conf/0,
	  fun monitor_conf_list/0
	 ]
	}.
-endif.

monitor_conf() ->
	Conf = #monitor_conf{id="test", 
						 name="test name", 
						 description="test description", 
						 type=syslog, 
						 facility_id="", 
						 calendar_id="", 
						 enabled=true, 
						 conf=""},
	?debugVal(Conf),
	
	DocIdx = riak_core_util:chash_key({<<"monitor_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_monitor),
	[IdxNode] = PrefList,
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf),
	
	Obj1 = kesto_monitor_vnode:get_conf(IdxNode, Conf#monitor_conf.id),
	?debugVal(Obj1),
	?assertMatch(Conf, Obj1),
	
	NewConf = Conf#monitor_conf{conf="test"},
	?debugVal(NewConf),
	ok = kesto_monitor_vnode:update_conf(IdxNode, NewConf),
	
	Obj2 = kesto_monitor_vnode:get_conf(IdxNode, NewConf#monitor_conf.id),
	?debugVal(Obj2),
	?assertMatch(NewConf, Obj2),
	
	ok = kesto_monitor_vnode:delete_conf(IdxNode, NewConf#monitor_conf.id),
	
	{error, notfound} = kesto_monitor_vnode:get_conf(IdxNode, NewConf#monitor_conf.id).

monitor_conf_list() ->
	Conf1 = #monitor_conf{id="test1", 
						  name="test1 name", 
						  description="test1 description", 
						  type=syslog, 
						  facility_id="", 
						  calendar_id="", 
						  enabled=true, 
						  conf=""},
	?debugVal(Conf1),
	
	Conf2 = Conf1#monitor_conf{id="test2", 
							   type=ping},
	?debugVal(Conf2),
	
	Conf3 = Conf1#monitor_conf{id="test3", 
							   type=syslog},
	?debugVal(Conf3),
	
	Conf4 = Conf1#monitor_conf{id="test4", 
							   type=ping},
	?debugVal(Conf4),
	
	DocIdx = riak_core_util:chash_key({<<"monitor_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_monitor),
	[IdxNode] = PrefList,
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf1),
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf2),
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf3),
	ok = kesto_monitor_vnode:put_conf(IdxNode, Conf4),
	
	List = kesto_monitor_vnode:get_conf_list(IdxNode, syslog),
	?debugVal(List),
	?assert(lists:member(Conf1, List)),
	?assert(lists:member(Conf3, List)).

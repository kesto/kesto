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

-module(job_conf_test).

-include_lib("kesto_core/include/kesto_test.hrl").
-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-ifdef(KESTO_JOB_TEST).
%% -define(RUN_TEST, true).
-endif.

setup() ->
	kesto_job_test_util:common_setup(?MODULE, fun configure/1).

cleanup() ->
	kesto_job_test_util:common_cleanup(?MODULE, fun configure/1).

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
	KestoJobSettings = [{check_interval, 10},
						{check_offset, 0},
						{ssh_exec_timeout, 86400000}],
	[ application:set_env(riak_core, K, V) || {K,V} <- CoreSettings ],
	[ application:set_env(riak_kv, K, V) || {K,V} <- KVSettings ],
	[ application:set_env(kesto_core, K, V) || {K,V} <- KestoCoreSettings ],
	[ application:set_env(kesto_job, K, V) || {K,V} <- KestoJobSettings ],
	ok;
configure(start) ->
	riak_core:wait_for_service(riak_kv),
	riak_core:wait_for_service(kesto_core),
	riak_core:wait_for_service(kesto_job);
configure(_) ->
	ok.

-ifdef(RUN_TEST).
job_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun job_conf/0,
	  fun job_conf_list/0,
	  fun job_relation_list/0
	 ]
	}.
-endif.

job_conf() ->
	DocIdx = riak_core_util:chash_key({<<"job_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_job),
	[IdxNode] = PrefList,
	
	Group1 = #job_conf{id="group1", 
					   name="group1 name", 
					   description="group1 description", 
					   type=group, 
					   group_id="group1", 
					   info_end_value_from=0,
					   info_end_value_to=0,
					   warning_end_value_from=1, 
					   warning_end_value_to=1},
	?debugVal(Group1),
	ok = kesto_job_vnode:put_conf(IdxNode, Group1),
	
	Net1 = #job_conf{id="net1", 
					 name="net1 name", 
					 description="net1 description", 
					 type=net, 
					 group_id="group1", 
					 parent_id="group1",
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Net1),
	ok = kesto_job_vnode:put_conf(IdxNode, Net1),
	
	Job1 = #job_conf{id="job1", 
					 name="job1 name", 
					 description="job1 description", 
					 type=job, 
					 group_id="group1", 
					 parent_id="net1",
					 next_id=[{"group1", "job2"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Job1),
	ok = kesto_job_vnode:put_conf(IdxNode, Job1),
	
	Job2 = #job_conf{id="job2", 
					 name="job2 name", 
					 description="job2 description", 
					 type=job, 
					 group_id="group1", 
					 parent_id="net1",
					 previous_id=[{"group1", "job1"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Job2),
	ok = kesto_job_vnode:put_conf(IdxNode, Job2),
	
	Obj1 = kesto_job_vnode:get_conf(IdxNode, Group1#job_conf.group_id, Group1#job_conf.id),
	?debugVal(Obj1),
	?assertMatch(Group1, Obj1),
	
	NewGroup1 = Group1#job_conf{description="group1 test"},
	?debugVal(NewGroup1),
	ok = kesto_job_vnode:update_conf(IdxNode, NewGroup1),
	
	Obj2 = kesto_job_vnode:get_conf(IdxNode, NewGroup1#job_conf.group_id, NewGroup1#job_conf.id),
	?debugVal(Obj2),
	?assertMatch(NewGroup1, Obj2),
	
	ok = kesto_job_vnode:delete_conf(IdxNode, NewGroup1#job_conf.group_id, NewGroup1#job_conf.id),
	
	{error, notfound} = kesto_job_vnode:get_conf(IdxNode, NewGroup1#job_conf.group_id, NewGroup1#job_conf.id).

job_conf_list() ->
	DocIdx = riak_core_util:chash_key({<<"job_conf">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_job),
	[IdxNode] = PrefList,
	
	Job1 = #job_conf{id="job1", 
					 name="job1 name", 
					 description="job1 description", 
					 type=group, 
					 group_id="group1", 
					 parent_id="net1",
					 next_id=[{"group1", "job2"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Job1),
	
	Job2 = Job1#job_conf{id="test2", 
						 type=net},
	?debugVal(Job2),
	
	Job3 = Job1#job_conf{id="test3", 
						 type=job},
	?debugVal(Job3),
	
	Job4 = Job1#job_conf{id="test4", 
						 type=job},
	?debugVal(Job4),
	
	ok = kesto_job_vnode:put_conf(IdxNode, Job1),
	ok = kesto_job_vnode:put_conf(IdxNode, Job2),
	ok = kesto_job_vnode:put_conf(IdxNode, Job3),
	ok = kesto_job_vnode:put_conf(IdxNode, Job4),
	
	List = kesto_job_vnode:get_conf_list(IdxNode, job),
	?debugVal(List),
	?assert(lists:member(Job3, List)),
	?assert(lists:member(Job4, List)),
	
	List2 = kesto_job_vnode:get_conf_list(IdxNode, job),
	?debugVal(List2),
	?assert(lists:member(Job3, List)),
	?assert(lists:member(Job4, List)),
	
	ok.

job_relation_list() ->
	Net1 = #job_conf{id="net1", 
					 name="net1 name", 
					 description="net1 description", 
					 type=net, 
					 group_id="group1", 
					 parent_id="group1", 
					 child_id=[{"group1", "job1"}, {"group1", "job2"}], 
					 previous_id=[], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Net1),
	ok = kesto_job_conf:put(Net1),
	
	Job1 = #job_conf{id="job1", 
					 name="job1 name", 
					 description="job1 description", 
					 type=job, 
					 group_id="group1", 
					 parent_id="net1",
					 child_id=[], 
					 previous_id=[], 
					 next_id=[{"group1", "job2"}], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Job1),
	ok = kesto_job_conf:put(Job1),
	
	Job2 = #job_conf{id="job2", 
					 name="job2 name", 
					 description="job2 description", 
					 type=job, 
					 group_id="group1", 
					 parent_id="net1",
					 child_id=[], 
					 previous_id=[{"group1", "job1"}], 
					 next_id=[], 
					 info_end_value_from=0,
					 info_end_value_to=0,
					 warning_end_value_from=1, 
					 warning_end_value_to=1},
	?debugVal(Job2),
	ok = kesto_job_conf:put(Job2),
	
	List = kesto_job_conf:get_relation_list(Net1#job_conf.group_id, Net1#job_conf.id),
	Dict = dict:from_list(List),
	?assertMatch(Net1, dict:fetch({"group1", "net1"}, Dict)),
	?assertMatch(Job1, dict:fetch({"group1", "job1"}, Dict)),
	?assertMatch(Job2, dict:fetch({"group1", "job2"}, Dict)),
	ok.

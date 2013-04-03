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

-module(job_session_test).

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
	  fun job_session/0,
	  fun job_session_list/0,
	  fun job_session_job/0,
	  fun job_session_job_list/0,
	  fun job_session_node/0,
	  fun job_session_node_list/0,
	  fun job_session_operation/0,
	  fun job_session_operation_list/0
	 ]
	}.
-endif.

job_session() ->
	Session1 = #job_session{id="session1", 
							job_id="net1",
							group_id="group1"},
	?debugVal(Session1),
	
	ok = kesto_job_session_info:put(Session1),
	{ok, Obj1} = kesto_job_session_info:get(Session1#job_session.id),
	?assertMatch(Session1, Obj1),
	
	NewSession1 = Session1#job_session{trigger_type=0},
	?debugVal(NewSession1),
	ok = kesto_job_session_info:update(NewSession1),
	
	{ok, Obj2} = kesto_job_session_info:get(NewSession1#job_session.id),
	?debugVal(Obj2),
	?assertMatch(NewSession1, Obj2),
	
	ok = kesto_job_session_info:delete(NewSession1#job_session.id),
	
	{error, notfound} = kesto_job_session_info:get(NewSession1#job_session.id),
	
	ok.

job_session_list() ->
	Session1 = #job_session{id="session1", 
							job_id="net1",
							group_id="group1",
							state=running},
	?debugVal(Session1),
	
	Session2 = #job_session{id="session2", 
							job_id="net2",
							group_id="group1",
							state=running},
	?debugVal(Session2),
	
	Session3 = #job_session{id="session3", 
							job_id="net3",
							group_id="group1",
							state='end'},
	?debugVal(Session3),
	
	Session4 = #job_session{id="session4", 
							job_id="net4",
							group_id="group1",
							state='end'},
	?debugVal(Session4),
	
	ok = kesto_job_session_info:put(Session1),
	ok = kesto_job_session_info:put(Session2),
	ok = kesto_job_session_info:put(Session3),
	ok = kesto_job_session_info:put(Session4),
	
	{ok, List1} = kesto_job_session_info:get_list(all, all),
	?debugVal(List1),
	?assert(lists:member(Session1, List1)),
	?assert(lists:member(Session2, List1)),
	?assert(lists:member(Session3, List1)),
	?assert(lists:member(Session4, List1)),
	
	{ok, List2} = kesto_job_session_info:get_list(running, all),
	?debugVal(List2),
	?assert(lists:member(Session1, List2)),
	?assert(lists:member(Session2, List2)),
	
	{ok, List3} = kesto_job_session_info:get_list('end', all),
	?debugVal(List3),
	?assert(lists:member(Session3, List3)),
	?assert(lists:member(Session4, List3)),
	
	ok.

job_session_job() ->
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
	SessionJob1 = #job_session_job{id="session1", 
								   job_id="net1",
								   group_id="group1",
								   conf=Net1},
	?debugVal(SessionJob1),
	
	ok = kesto_job_session_info:put(SessionJob1),
	{ok, Obj1} = kesto_job_session_info:get(SessionJob1#job_session_job.id, 
										   SessionJob1#job_session_job.group_id, 
										   SessionJob1#job_session_job.job_id),
	?assertMatch(SessionJob1, Obj1),
	
	NewSessionJob1 = SessionJob1#job_session_job{state=start},
	?debugVal(NewSessionJob1),
	ok = kesto_job_session_info:update(NewSessionJob1),
	
	{ok, Obj2} = kesto_job_session_info:get(NewSessionJob1#job_session_job.id, 
										   NewSessionJob1#job_session_job.group_id, 
										   NewSessionJob1#job_session_job.job_id),
	?debugVal(Obj2),
	?assertMatch(NewSessionJob1, Obj2),
	
	ok = kesto_job_session_info:delete(NewSessionJob1#job_session_job.id, 
									  NewSessionJob1#job_session_job.group_id, 
									  NewSessionJob1#job_session_job.job_id),
	
	{error, notfound} = kesto_job_session_info:get(NewSessionJob1#job_session_job.id, 
												  NewSessionJob1#job_session_job.group_id, 
												  NewSessionJob1#job_session_job.job_id),
	ok.

job_session_job_list() ->
	SessionJob1 = #job_session_job{id="session1", 
								   job_id="job1",
								   group_id="group1",
								   state='end'},
	?debugVal(SessionJob1),
	
	SessionJob2 = #job_session_job{id="session1", 
								   job_id="job2",
								   group_id="group1",
								   state=running},
	?debugVal(SessionJob2),
	
	SessionJob3 = #job_session_job{id="session2", 
								   job_id="job1",
								   group_id="group1",
								   state='end'},
	?debugVal(SessionJob3),
	
	SessionJob4 = #job_session_job{id="session2", 
								   job_id="job2",
								   group_id="group1",
								   state=running},
	?debugVal(SessionJob4),
	
	ok = kesto_job_session_info:put(SessionJob1),
	ok = kesto_job_session_info:put(SessionJob2),
	ok = kesto_job_session_info:put(SessionJob3),
	ok = kesto_job_session_info:put(SessionJob4),
	
	{ok, List1} = kesto_job_session_info:get_list("session1", all, all),
	?debugVal(List1),
	?assert(lists:member(SessionJob1, List1)),
	?assert(lists:member(SessionJob2, List1)),
	
	{ok, List2} = kesto_job_session_info:get_list("session1", running, all),
	?debugVal(List2),
	?assert(lists:member(SessionJob2, List2)),
	
	{ok, List3} = kesto_job_session_info:get_list("session1", 'end', all),
	?debugVal(List3),
	?assert(lists:member(SessionJob1, List3)),
	
	ok.

job_session_node() ->
	SessionNode1 = #job_session_node{id="session1", 
									 job_id="net1",
									 group_id="group1",
									 facility=#node{id="node1",
													name="node1 name"},
									 state=start},
	?debugVal(SessionNode1),
	
	ok = kesto_job_session_info:put(SessionNode1),
	{ok, Obj1} = kesto_job_session_info:get(SessionNode1#job_session_node.id, 
											SessionNode1#job_session_node.group_id, 
											SessionNode1#job_session_node.job_id,
											SessionNode1#job_session_node.facility#node.id),
	?assertMatch(SessionNode1, Obj1),
	
	NewSessionNode1 = SessionNode1#job_session_node{state='end'},
	?debugVal(NewSessionNode1),
	ok = kesto_job_session_info:update(NewSessionNode1),
	
	{ok, Obj2} = kesto_job_session_info:get(NewSessionNode1#job_session_node.id, 
											NewSessionNode1#job_session_node.group_id, 
											NewSessionNode1#job_session_node.job_id,
											NewSessionNode1#job_session_node.facility#node.id),
	?debugVal(Obj2),
	?assertMatch(NewSessionNode1, Obj2),
	
	ok = kesto_job_session_info:delete(NewSessionNode1#job_session_node.id, 
									   NewSessionNode1#job_session_node.group_id, 
									   NewSessionNode1#job_session_node.job_id,
									   NewSessionNode1#job_session_node.facility#node.id),
	
	{error, notfound} = kesto_job_session_info:get(NewSessionNode1#job_session_node.id, 
												   NewSessionNode1#job_session_node.group_id, 
												   NewSessionNode1#job_session_node.job_id,
												   NewSessionNode1#job_session_node.facility#node.id),
	ok.

job_session_node_list() ->
	SessionNode1 = #job_session_node{id="session1", 
									 job_id="job1",
									 group_id="group1",
									 facility=#node{id="node1",
													name="node1 name"},
									 state=start},
	?debugVal(SessionNode1),
	
	SessionNode2 = #job_session_node{id="session1", 
									 job_id="job2",
									 group_id="group1",
									 facility=#node{id="node1",
													name="node1 name"},
									 state=start},
	?debugVal(SessionNode2),
	
	SessionNode3 = #job_session_node{id="session2", 
									 job_id="job1",
									 group_id="group1",
									 facility=#node{id="node1",
													name="node1 name"},
									 state=start},
	?debugVal(SessionNode3),
	
	SessionNode4 = #job_session_node{id="session2", 
									 job_id="job2",
									 group_id="group1",
									 facility=#node{id="node1",
													name="node1 name"},
									 state=start},
	?debugVal(SessionNode4),
	
	ok = kesto_job_session_info:put(SessionNode1),
	ok = kesto_job_session_info:put(SessionNode2),
	ok = kesto_job_session_info:put(SessionNode3),
	ok = kesto_job_session_info:put(SessionNode4),
	
	{ok, List1} = kesto_job_session_info:get_list(all, all),
	?debugVal(List1),
	?assert(lists:member(SessionNode1, List1)),
	?assert(lists:member(SessionNode2, List1)),
	?assert(lists:member(SessionNode3, List1)),
	?assert(lists:member(SessionNode4, List1)),
	
	ok.

job_session_operation() ->
	SessionOperation1 = #job_session_operation{id="operation1", 
											   session_id="session1",
											   job_id="job1",
											   group_id="group1",
											   facility_id="node1",
											   operation=stop,
											   end_value=0,
											   message="",
											   create_timestamp={{2012,7,1},{12,0,0}}},
	?debugVal(SessionOperation1),
	
	ok = kesto_job_session_operation:put(SessionOperation1),
	{ok, Obj1} = kesto_job_session_operation:get(SessionOperation1#job_session_operation.id),
	?assertMatch(SessionOperation1, Obj1),
	
	NewSessionOperation1 = SessionOperation1#job_session_operation{create_timestamp={{2012,7,1},{12,1,0}}},
	?debugVal(NewSessionOperation1),
	ok = kesto_job_session_operation:update(NewSessionOperation1),
	
	{ok, Obj2} = kesto_job_session_operation:get(NewSessionOperation1#job_session_operation.id),
	?debugVal(Obj2),
	?assertMatch(NewSessionOperation1, Obj2),
	
	ok = kesto_job_session_operation:delete(NewSessionOperation1#job_session_operation.id),
	
	{error, notfound} = kesto_job_session_operation:get(NewSessionOperation1#job_session_operation.id),
	
	ok.

job_session_operation_list() ->
	SessionOperation1 = #job_session_operation{id="operation1", 
											   session_id="session1",
											   job_id="job1",
											   group_id="group1",
											   facility_id="node1",
											   operation=stop,
											   state=running,
											   end_value=0,
											   message="",
											   create_timestamp={{2012,7,1},{12,0,0}}},
	?debugVal(SessionOperation1),
	
	SessionOperation2 = #job_session_operation{id="operation2", 
											   session_id="session1",
											   job_id="job1",
											   group_id="group1",
											   facility_id="node1",
											   operation=change,
											   state=wait,
											   end_value=0,
											   message="",
											   create_timestamp={{2012,7,1},{12,1,0}}},
	?debugVal(SessionOperation2),
	
	SessionOperation3 = #job_session_operation{id="operation3", 
											   session_id="session2",
											   job_id="job1",
											   group_id="group1",
											   facility_id="node1",
											   operation=stop,
											   state=running,
											   end_value=0,
											   message="",
											   create_timestamp={{2012,7,1},{12,2,0}}},
	?debugVal(SessionOperation3),
	
	SessionOperation4 = #job_session_operation{id="operation4", 
											   session_id="session2",
											   job_id="job1",
											   group_id="group1",
											   facility_id="node1",
											   operation=change,
											   state=wait,
											   end_value=0,
											   message="",
											   create_timestamp={{2012,7,1},{12,3,0}}},
	?debugVal(SessionOperation4),
	
	ok = kesto_job_session_operation:put(SessionOperation1),
	ok = kesto_job_session_operation:put(SessionOperation2),
	ok = kesto_job_session_operation:put(SessionOperation3),
	ok = kesto_job_session_operation:put(SessionOperation4),
	
	{ok, List1} = kesto_job_session_operation:get_list(all),
	?debugVal(List1),
	?assert(lists:member(SessionOperation1, List1)),
	?assert(lists:member(SessionOperation2, List1)),
	?assert(lists:member(SessionOperation3, List1)),
	?assert(lists:member(SessionOperation4, List1)),
	[SessionOperation1, SessionOperation2, SessionOperation3, SessionOperation4] = List1,
	
	{ok, List2} = kesto_job_session_operation:get_list(running),
	?debugVal(List2),
	?assert(lists:member(SessionOperation1, List2)),
	?assert(lists:member(SessionOperation3, List2)),
	
	{ok, List3} = kesto_job_session_operation:get_list("session1", all),
	?debugVal(List3),
	?assert(lists:member(SessionOperation1, List3)),
	?assert(lists:member(SessionOperation2, List3)),
	
	{ok, List4} = kesto_job_session_operation:get_list("session1", running),
	?debugVal(List4),
	?assert(lists:member(SessionOperation1, List4)),

	{ok, List5} = kesto_job_session_operation:get_list("session1", "group1", "job1", running),
	?debugVal(List5),
	?assert(lists:member(SessionOperation1, List5)),

	{ok, List6} = kesto_job_session_operation:get_list("session1", "group1", "job1", "node1", all),
	?debugVal(List6),
	?assert(lists:member(SessionOperation1, List6)),
	?assert(lists:member(SessionOperation2, List6)),

	{ok, List7} = kesto_job_session_operation:get_list("session1", "group1", "job1", "node1", running),
	?debugVal(List7),
	?assert(lists:member(SessionOperation1, List7)),

	{ok, List8} = kesto_job_session_operation:get_list("session1", "group1", "job1", "node1", 'end'),
	?debugVal(List8),
	?assert(erlang:length(List8) == 0),
	
	ok.

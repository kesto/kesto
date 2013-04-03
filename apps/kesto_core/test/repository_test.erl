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

-module(repository_test).

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
						 {smtp, []},
						 {mail_from, ""},
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
repository_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun node/0,
	  fun node_error/0,
	  fun scope/0,
	  fun scope_error/0,
	  fun assign/0,
	  fun assign_error/0,
	  fun is_node/0,
	  fun get_list1/0,
	  fun get_list2/0,
	  fun get_list3/0
	 ]
	}.
-endif.

node() ->
	Node = #node{id="test", 
				 name="test name", 
				 description="test description", 
				 type=linux, 
				 enabled=true, 
				 ipv4=["127.0.0.1"], 
				 ipv6=[], 
				 hostname=["testhost1"], 
				 create_timestamp={{2012,6,5},{11,30,14}},
				 update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Node),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_repository_vnode:put(IdxNode, Node),
	
	Obj1 = kesto_core_repository_vnode:get(IdxNode, Node#node.id),
	?debugVal(Obj1),
	?assertMatch(Node, Obj1),
	
	NewNode = Node#node{name="test name 2"},
	?debugVal(NewNode),
	ok = kesto_core_repository_vnode:update(IdxNode, NewNode),
	
	Obj2 = kesto_core_repository_vnode:get(IdxNode, NewNode#node.id),
	?debugVal(Obj2),
	?assertMatch(NewNode, Obj2),
	
	ok = kesto_core_repository_vnode:delete(IdxNode, NewNode#node.id),
	
	{error, notfound} = kesto_core_repository_vnode:get(IdxNode, NewNode#node.id).

node_error() ->
	Node = #node{id="test", 
				 name="test name", 
				 description="test description", 
				 type=linux, 
				 enabled=true, 
				 ipv4=["127.0.0.1"], 
				 ipv6=[], 
				 hostname=["testhost1"], 
				 create_timestamp={{2012,6,5},{11,30,14}},
				 update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Node),
	
	{error, notfound} = kesto_core_repository:get(Node#node.id),
	{error, notfound} = kesto_core_repository:update(Node),
	{error, notfound} = kesto_core_repository:delete(Node#node.id),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_repository_vnode:get(IdxNode, Node#node.id),
	{error, notfound} = kesto_core_repository_vnode:update(IdxNode, Node),
	{error, notfound} = kesto_core_repository_vnode:delete(IdxNode, Node#node.id).

scope() ->
	Scope = #scope{id="test", 
				   name="test name", 
				   description="test description", 
				   enabled=true, 
				   create_timestamp={{2012,6,5},{11,30,14}},
				   update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope),
	
	DocIdx = riak_core_util:chash_key({<<"scope">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_repository_vnode:put(IdxNode, Scope),
	
	Obj1 = kesto_core_repository_vnode:get(IdxNode, Scope#scope.id),
	?debugVal(Obj1),
	?assertMatch(Scope, Obj1),
	
	NewScope = Scope#scope{name="test name 2"},
	?debugVal(NewScope),
	ok = kesto_core_repository_vnode:update(IdxNode, NewScope),
	
	Obj2 = kesto_core_repository_vnode:get(IdxNode, NewScope#scope.id),
	?debugVal(Obj2),
	?assertMatch(NewScope, Obj2),
	
	ok = kesto_core_repository_vnode:delete(IdxNode, NewScope#scope.id),
	
	{error, notfound} = kesto_core_repository_vnode:get(IdxNode, NewScope#scope.id).

scope_error() ->
	Scope = #scope{id="test", 
				   name="test name", 
				   description="test description", 
				   enabled=true, 
				   create_timestamp={{2012,6,5},{11,30,14}},
				   update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope),
	
	{error, notfound} = kesto_core_repository:get(Scope#scope.id),
	{error, notfound} = kesto_core_repository:update(Scope),
	{error, notfound} = kesto_core_repository:delete(Scope#scope.id),
	
	DocIdx = riak_core_util:chash_key({<<"scope">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_repository_vnode:get(IdxNode, Scope#scope.id),
	{error, notfound} = kesto_core_repository_vnode:update(IdxNode, Scope),
	{error, notfound} = kesto_core_repository_vnode:delete(IdxNode, Scope#scope.id).

assign() ->
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
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_repository_vnode:put(IdxNode, Node1),
	
	Node2 = Node1#node{id="node2", 
					   name="node2 name", 
					   description="node2 description"},
	?debugVal(Node2),
	ok = kesto_core_repository_vnode:put(IdxNode, Node2),
	
	Scope1 = #scope{id="scope1", 
					name="scope1 name", 
					description="scope1 description", 
					enabled=true, 
					children=[], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	
	ok = kesto_core_repository_vnode:put(IdxNode, Scope1),
	
	ok = kesto_core_repository_vnode:assign(IdxNode, Scope1#scope.id, ["node1", "node2"]),
	
	Obj1 = kesto_core_repository_vnode:get(IdxNode, Scope1#scope.id),
	?debugVal(Obj1),
	?assertEqual(Obj1#scope.children, ["node1", "node2"]),
	
	ok = kesto_core_repository_vnode:unassign(IdxNode, Scope1#scope.id, ["node1", "node2"]),
	
	Obj2 = kesto_core_repository_vnode:get(IdxNode, Scope1#scope.id),
	?debugVal(Obj2),
	?assertEqual(Obj2#scope.children, []).

assign_error() ->
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
					   description="node2 description"},
	?debugVal(Node2),
	ok = kesto_core_repository:put(Node2),
	
	Scope1 = #scope{id="scope1", 
					name="scope1 name", 
					description="scope1 description", 
					enabled=true, 
					children=[], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	
	{error, notfound} = kesto_core_repository:assign(Scope1#scope.id, ["node1", "node2"]),
	{error, notfound} = kesto_core_repository:unassign(Scope1#scope.id, ["node1", "node2"]),
	
	DocIdx = riak_core_util:chash_key({<<"scope">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_repository_vnode:assign(IdxNode, Scope1#scope.id, ["node1", "node2"]),
	{error, notfound} = kesto_core_repository_vnode:unassign(IdxNode, Scope1#scope.id, ["node1", "node2"]),
	
	ok = kesto_core_repository:put(Scope1),
	{error, notfound} = kesto_core_repository:assign(Scope1#scope.id, ["node1", "node3"]),
	
	ok = kesto_core_repository:put(Scope1),
	{error, notfound} = kesto_core_repository:unassign(Scope1#scope.id, ["node1", "node3"]).

is_node() ->
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
					children=[], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	ok = kesto_core_repository:put(Scope1),
	
	Scope2 = Scope1#scope{id="scope2", 
						  name="scope2 name", 
						  description="scope2 description", 
						  enabled=false},
	?debugVal(Scope2),
	ok = kesto_core_repository:put(Scope2),
	
	IsNode1 = kesto_core_repository:is_node("node1", all),
	?debugVal(IsNode1),
	?assert(IsNode1),
	IsNode2 = kesto_core_repository:is_node("node2", all),
	?debugVal(IsNode2),
	?assert(IsNode2),
	IsNode3 = kesto_core_repository:is_node("scope1", all),
	?debugVal(IsNode3),
	?assertNot(IsNode3),
	IsNode4 = kesto_core_repository:is_node("scope2", all),
	?debugVal(IsNode4),
	?assertNot(IsNode4),
	IsNode5 = kesto_core_repository:is_node("node1", true),
	?debugVal(IsNode5),
	?assert(IsNode5),
	IsNode6 = kesto_core_repository:is_node("node2", true),
	?debugVal(IsNode6),
	?assertNot(IsNode6),
	IsNode7 = kesto_core_repository:is_node("node1", false),
	?debugVal(IsNode7),
	?assertNot(IsNode7),
	IsNode8 = kesto_core_repository:is_node("node2", false),
	?debugVal(IsNode8),
	?assert(IsNode8),
	
	IsScope1 = kesto_core_repository:is_scope("node1", all),
	?debugVal(IsScope1),
	?assertNot(IsScope1),
	IsScope2 = kesto_core_repository:is_scope("node2", all),
	?debugVal(IsScope2),
	?assertNot(IsScope2),
	IsScope3 = kesto_core_repository:is_scope("scope1", all),
	?debugVal(IsScope3),
	?assert(IsScope3),
	IsScope4 = kesto_core_repository:is_scope("scope2", all),
	?debugVal(IsScope4),
	?assert(IsScope4),
	IsScope5 = kesto_core_repository:is_scope("scope1", true),
	?debugVal(IsScope5),
	?assert(IsScope5),
	IsScope6 = kesto_core_repository:is_scope("scope2", true),
	?debugVal(IsScope6),
	?assertNot(IsScope6),
	IsScope7 = kesto_core_repository:is_scope("scope1", false),
	?debugVal(IsScope7),
	?assertNot(IsScope7),
	IsScope8 = kesto_core_repository:is_scope("scope2", false),
	?debugVal(IsScope8),
	?assert(IsScope8).

get_list1() ->
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
					children=[], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	ok = kesto_core_repository:put(Scope1),
	
	Scope2 = Scope1#scope{id="scope2", 
						  name="scope2 name", 
						  description="scope2 description", 
						  enabled=false},
	?debugVal(Scope2),
	ok = kesto_core_repository:put(Scope2),
	
	{ok, IDList1} = kesto_core_repository:get_id_list(node, all),
	?debugVal(IDList1),
	?assert(lists:member("node1", IDList1)),
	?assert(lists:member("node2", IDList1)),
	
	{ok, IDList2} = kesto_core_repository:get_id_list(node, true),
	?debugVal(IDList2),
	?assert(lists:member("node1", IDList2)),
	?assertNot(lists:member("node2", IDList2)),
	
	{ok, IDList3} = kesto_core_repository:get_id_list(node, false),
	?debugVal(IDList3),
	?assertNot(lists:member("node1", IDList3)),
	?assert(lists:member("node2", IDList3)),
	
	{ok, IDList4} = kesto_core_repository:get_id_list(scope, all),
	?debugVal(IDList4),
	?assert(lists:member("scope1", IDList4)),
	?assert(lists:member("scope2", IDList4)),
	
	{ok, IDList5} = kesto_core_repository:get_id_list(scope, true),
	?debugVal(IDList5),
	?assert(lists:member("scope1", IDList5)),
	?assertNot(lists:member("scope2", IDList5)),
	
	{ok, IDList6} = kesto_core_repository:get_id_list(scope, false),
	?debugVal(IDList6),
	?assertNot(lists:member("scope1", IDList6)),
	?assert(lists:member("scope2", IDList6)),
	
	{ok, List1} = kesto_core_repository:get_list(node, all),
	?debugVal(List1),
	?assert(lists:member(Node1, List1)),
	?assert(lists:member(Node2, List1)),
	
	{ok, List2} = kesto_core_repository:get_list(node, true),
	?debugVal(List2),
	?assert(lists:member(Node1, List2)),
	?assertNot(lists:member(Node2, List2)),
	
	{ok, List3} = kesto_core_repository:get_list(node, false),
	?debugVal(List3),
	?assertNot(lists:member(Node1, List3)),
	?assert(lists:member(Node2, List3)),
	
	{ok, List4} = kesto_core_repository:get_list(scope, all),
	?debugVal(List4),
	?assert(lists:member(Scope1, List4)),
	?assert(lists:member(Scope2, List4)),
	
	{ok, List5} = kesto_core_repository:get_list(scope, true),
	?debugVal(List5),
	?assert(lists:member(Scope1, List5)),
	?assertNot(lists:member(Scope2, List5)),
	
	{ok, List6} = kesto_core_repository:get_list(scope, false),
	?debugVal(List6),
	?assertNot(lists:member(Scope1, List6)),
	?assert(lists:member(Scope2, List6)).

get_list2() ->
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
					children=[], 
					create_timestamp={{2012,6,5},{11,30,14}},
					update_timestamp={{2012,6,5},{11,33,14}}},
	?debugVal(Scope1),
	ok = kesto_core_repository:put(Scope1),
	
	Scope2 = Scope1#scope{id="scope2", 
						  name="scope2 name", 
						  description="scope2 description", 
						  enabled=false},
	?debugVal(Scope2),
	ok = kesto_core_repository:put(Scope2),
	
	{ok, List1} = kesto_core_repository:get_facility_id("testhost1", all),
	?debugVal(List1),
	?assert(lists:member("node1", List1)),
	?assertNot(lists:member("node2", List1)),
	{ok, List2} = kesto_core_repository:get_facility_id("testhost1", true),
	?debugVal(List2),
	?assert(lists:member("node1", List2)),
	?assertNot(lists:member("node2", List2)),
	{ok, List3} = kesto_core_repository:get_facility_id("testhost1", false),
	?debugVal(List3),
	?assertNot(lists:member("node1", List3)),
	?assertNot(lists:member("node2", List3)),
	
	{ok, List4} = kesto_core_repository:get_facility_id("127.0.0.1", all),
	?debugVal(List4),
	?assert(lists:member("node1", List4)),
	?assert(lists:member("node2", List4)),
	{ok, List5} = kesto_core_repository:get_facility_id("127.0.0.1", true),
	?debugVal(List5),
	?assert(lists:member("node1", List5)),
	?assertNot(lists:member("node2", List5)),
	{ok, List6} = kesto_core_repository:get_facility_id("127.0.0.1", false),
	?debugVal(List6),
	?assertNot(lists:member("node1", List6)),
	?assert(lists:member("node2", List6)).

get_list3() ->
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
	
	Scope2 = Scope1#scope{id="scope2", 
						  name="scope2 name", 
						  description="scope2 description", 
						  enabled=false},
	?debugVal(Scope2),
	ok = kesto_core_repository:put(Scope2),
	
	{ok, List1} = kesto_core_repository:get_node_facility_id_list("scope1", all),
	?debugVal(List1),
	?assert(lists:member("node1", List1)),
	?assert(lists:member("node2", List1)),
	{ok, List2} = kesto_core_repository:get_node_facility_id_list("scope1", true),
	?debugVal(List2),
	?assert(lists:member("node1", List2)),
	?assertNot(lists:member("node2", List2)),
	{ok, List3} = kesto_core_repository:get_node_facility_id_list("scope1", false),
	?debugVal(List3),
	?assertNot(lists:member("node1", List3)),
	?assertNot(lists:member("node2", List3)),
	{ok, List4} = kesto_core_repository:get_node_facility_id_list("node1", all),
	?debugVal(List4),
	?assert(lists:member("node1", List4)),
	{ok, List5} = kesto_core_repository:get_node_facility_id_list("node1", true),
	?debugVal(List5),
	?assert(lists:member("node1", List5)),
	{ok, List6} = kesto_core_repository:get_node_facility_id_list("node1", false),
	?debugVal(List6),
	?assertNot(lists:member("node1", List6)).

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

-module(auth_test).

-include("kesto_test.hrl").
-include("kesto_core.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-ifdef(KESTO_CORE_TEST).
-define(RUN_TEST, true).
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
						 {ssh_client_connect_timeout, 5000},
						 {expiration_date, 86400}],
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
event_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun system/0,
	  fun system_error/0,
	  fun roll/0,
	  fun roll_error/0,
	  fun user/0,
	  fun user_error/0,
	  fun sign_in/0,
	  fun sign_out/0,
	  fun is_authenticated/0
	 ]
	}.
-endif.

system() ->
	System = #system{id="test", 
					 description="test description", 
					 create_timestamp={{2012,10,5},{11,30,14}},
					 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(System),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxSystem] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxSystem, System),
	
	Obj1 = kesto_core_auth_vnode:get(IdxSystem, system, System#system.id),
	?debugVal(Obj1),
	?assertMatch(System, Obj1),
	
	NewSystem = System#system{description="test description 2"},
	?debugVal(NewSystem),
	ok = kesto_core_auth_vnode:update(IdxSystem, NewSystem),
	
	Obj2 = kesto_core_auth_vnode:get(IdxSystem, system, NewSystem#system.id),
	?debugVal(Obj2),
	?assertMatch(NewSystem, Obj2),
	
	ok = kesto_core_auth_vnode:delete(IdxSystem, system, NewSystem#system.id),
	
	{error, notfound} = kesto_core_auth_vnode:get(IdxSystem, system, NewSystem#system.id).

system_error() ->
	System = #system{id="test", 
					 description="test description", 
					 create_timestamp={{2012,10,5},{11,30,14}},
					 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(System),
	
	{error, notfound} = kesto_core_auth:get(system, System#system.id),
	{error, notfound} = kesto_core_auth:update(System),
	{error, notfound} = kesto_core_auth:delete(system, System#system.id),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxSystem] = PrefList,
	{error, notfound} = kesto_core_auth_vnode:get(IdxSystem, system, System#system.id),
	{error, notfound} = kesto_core_auth_vnode:update(IdxSystem, System),
	{error, notfound} = kesto_core_auth_vnode:delete(IdxSystem, system, System#system.id).

roll() ->
	Roll = #roll{id="test", 
				 description="test description", 
				 system_id="system1",
				 auth=[{job, [read, write]}], 
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(Roll),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxSystem] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxSystem, Roll),
	
	Obj1 = kesto_core_auth_vnode:get(IdxSystem, roll, Roll#roll.id),
	?debugVal(Obj1),
	?assertMatch(Roll, Obj1),
	
	NewRoll = Roll#roll{description="test description 2"},
	?debugVal(NewRoll),
	ok = kesto_core_auth_vnode:update(IdxSystem, NewRoll),
	
	Obj2 = kesto_core_auth_vnode:get(IdxSystem, roll, NewRoll#roll.id),
	?debugVal(Obj2),
	?assertMatch(NewRoll, Obj2),
	
	ok = kesto_core_auth_vnode:delete(IdxSystem, roll, NewRoll#roll.id),
	
	{error, notfound} = kesto_core_auth_vnode:get(IdxSystem, roll, NewRoll#roll.id).

roll_error() ->
	Roll = #roll{id="test", 
				 description="test description", 
				 system_id="system1",
				 auth=[{job, [read, write]}], 
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(Roll),
	
	{error, notfound} = kesto_core_auth:get(roll, Roll#roll.id),
	{error, notfound} = kesto_core_auth:update(Roll),
	{error, notfound} = kesto_core_auth:delete(roll, Roll#roll.id),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxRoll] = PrefList,
	{error, notfound} = kesto_core_auth_vnode:get(IdxRoll, roll, Roll#roll.id),
	{error, notfound} = kesto_core_auth_vnode:update(IdxRoll, Roll),
	{error, notfound} = kesto_core_auth_vnode:delete(IdxRoll, roll, Roll#roll.id).

user() ->
	User = #user{id="test", 
				 password="password",
				 description="test description", 
				 mail="test@test.com",
				 roll=["roll"],
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(User),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxUser] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxUser, User),
	
	Obj1 = kesto_core_auth_vnode:get(IdxUser, user, User#user.id),
	?debugVal(Obj1),
	?assertMatch(User, Obj1),
	
	NewUser = User#user{description="test description 2"},
	?debugVal(NewUser),
	ok = kesto_core_auth_vnode:update(IdxUser, NewUser),
	
	Obj2 = kesto_core_auth_vnode:get(IdxUser, user, NewUser#user.id),
	?debugVal(Obj2),
	?assertMatch(NewUser, Obj2),
	
	ok = kesto_core_auth_vnode:delete(IdxUser, user, NewUser#user.id),
	
	{error, notfound} = kesto_core_auth_vnode:get(IdxUser, user, NewUser#user.id).

user_error() ->
	User = #user{id="test", 
				 password="password",
				 description="test description", 
				 mail="test@test.com",
				 roll=["roll"],
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(User),
	
	{error, notfound} = kesto_core_auth:get(user, User#user.id),
	{error, notfound} = kesto_core_auth:update(User),
	{error, notfound} = kesto_core_auth:delete(user, User#user.id),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxUser] = PrefList,
	{error, notfound} = kesto_core_auth_vnode:get(IdxUser, user, User#user.id),
	{error, notfound} = kesto_core_auth_vnode:update(IdxUser, User),
	{error, notfound} = kesto_core_auth_vnode:delete(IdxUser, user, User#user.id).

sign_in() ->
	User = #user{id="test", 
				 password="password",
				 description="test description", 
				 mail="test@test.com",
				 roll=["roll"],
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(User),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxUser] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxUser, User),
	
	Obj1 = kesto_core_auth_vnode:sign_in(IdxUser, "test", "password", "Test", "192.168.0.1"),
	?debugVal(Obj1),
	
	{error, unmatch} = kesto_core_auth_vnode:sign_in(IdxUser, "test", "test", "Test", "192.168.0.1"),
	
	{error, notfound} = kesto_core_auth_vnode:sign_in(IdxUser, "test1", "password", "Test", "192.168.0.1").

sign_out() ->
	User = #user{id="test", 
				 password="password",
				 description="test description", 
				 mail="test@test.com",
				 roll=["roll"],
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(User),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxUser] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxUser, User),
	
	Obj1 = kesto_core_auth_vnode:sign_in(IdxUser, "test", "password", "Test", "192.168.0.1"),
	?debugVal(Obj1),
	
	ok = kesto_core_auth_vnode:sign_out(IdxUser, Obj1#auth_session.session_id),

	Obj2 = kesto_core_auth_vnode:sign_in(IdxUser, "test", "password", "Test", "192.168.0.1"),
	?debugVal(Obj2),
	?assertNotEqual(Obj1#auth_session.session_id, Obj2#auth_session.session_id),
	
	{error, notfound} = kesto_core_auth_vnode:sign_out(IdxUser, "testtesttest").

is_authenticated() ->
	User = #user{id="test", 
				 password="password",
				 description="test description", 
				 mail="test@test.com",
				 roll=["roll"],
				 create_timestamp={{2012,10,5},{11,30,14}},
				 update_timestamp={{2012,10,5},{11,33,14}}},
	?debugVal(User),
	
	DocIdx = riak_core_util:chash_key({<<"node">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxUser] = PrefList,
	ok = kesto_core_auth_vnode:put(IdxUser, User),
	
	Obj1 = kesto_core_auth_vnode:sign_in(IdxUser, "test", "password", "Test", "192.168.0.1"),
	?debugVal(Obj1),
	
	Obj2 = kesto_core_auth_vnode:is_authenticated(IdxUser, Obj1#auth_session.session_id, "Test", "192.168.0.1"),
	?debugVal(Obj2),
	?assertEqual(Obj1#auth_session.session_id, Obj2#auth_session.session_id),

	{error, unmatch} = kesto_core_auth_vnode:is_authenticated(IdxUser, Obj1#auth_session.session_id, "Test", "192.168.0.2"),
	
	{error, unmatch} = kesto_core_auth_vnode:is_authenticated(IdxUser, Obj1#auth_session.session_id, "Test1", "192.168.0.1"),
	
	{error, notfound} = kesto_core_auth_vnode:is_authenticated(IdxUser, "testtest", "Test", "192.168.0.1").
	
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

-module(event_test).

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
event_test_() ->
	{foreach,
	 setup(),
	 cleanup(),
	 [
	  fun event/0,
	  fun event_error/0,
	  fun get_list/0
	 ]
	}.
-endif.

event() ->
	Event = #event{id=kesto_core_event:get_id(),
				   priority=info, 
				   timestamp={{2012,6,5},{11,30,14}}, 
				   timestamp_raw={{2012,6,5},{11,30,10}}, 
				   monitor_type=syslog, 
				   monitor_id="test", 
				   facility_id="node1", 
				   facility_name="node1 name", 
				   message="message", 
				   org_message="org_message",
				   check=false, 
				   comment=""},
	?debugVal(Event),
	
	DocIdx = riak_core_util:chash_key({<<"event">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	ok = kesto_core_event_vnode:put(IdxNode, Event),
	
	Obj1 = kesto_core_event_vnode:get(IdxNode, Event#event.id),
	?debugVal(Obj1),
	?assertMatch(Event, Obj1),
	
	NewEvent = Event#event{check=true,
						   comment="comment"},
	?debugVal(NewEvent),
	ok = kesto_core_event_vnode:update(IdxNode, NewEvent),
	
	Obj2 = kesto_core_event_vnode:get(IdxNode, NewEvent#event.id),
	?debugVal(Obj2),
	?assertMatch(NewEvent, Obj2),
	
	ok = kesto_core_event_vnode:delete(IdxNode, NewEvent#event.id),
	
	{error, notfound} = kesto_core_event_vnode:get(IdxNode, NewEvent#event.id).

event_error() ->
	Event = #event{id=kesto_core_event:get_id(),
				   priority=info, 
				   timestamp={{2012,6,5},{11,30,14}}, 
				   timestamp_raw={{2012,6,5},{11,30,10}}, 
				   monitor_type=syslog, 
				   monitor_id="test", 
				   facility_id="node1", 
				   facility_name="node1 name", 
				   message="message", 
				   org_message="org_message",
				   check=false, 
				   comment=""},
	?debugVal(Event),
	
	{error, notfound} = kesto_core_event:get(Event#event.id),
	{error, notfound} = kesto_core_event:update(Event),
	{error, notfound} = kesto_core_event:delete(Event#event.id),
	
	DocIdx = riak_core_util:chash_key({<<"event">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	{error, notfound} = kesto_core_event_vnode:get(IdxNode, Event#event.id),
	{error, notfound} = kesto_core_event_vnode:update(IdxNode, Event),
	{error, notfound} = kesto_core_event_vnode:delete(IdxNode, Event#event.id).

get_list() ->
	Event1 = #event{id=kesto_core_event:get_id(),
					priority=info, 
					timestamp={{2012,6,5},{11,30,14}}, 
					timestamp_raw={{2012,6,5},{11,30,10}}, 
					monitor_type=syslog, 
					monitor_id="test", 
					facility_id="node1", 
					facility_name="node1 name", 
					message="message", 
					org_message="org_message",
					check=false, 
					comment=""},
	?debugVal(Event1),
	ok = kesto_core_event:put(Event1),
	
	Event2 = #event{id=kesto_core_event:get_id(),
					priority=info, 
					timestamp={{2012,6,5},{11,30,14}}, 
					timestamp_raw={{2012,6,5},{11,30,10}}, 
					monitor_type=syslog, 
					monitor_id="test", 
					facility_id="node1", 
					facility_name="node1 name", 
					message="message", 
					org_message="org_message",
					check=false, 
					comment=""},
	?debugVal(Event2),
	ok = kesto_core_event:put(Event2),
	
	DocIdx = riak_core_util:chash_key({<<"event">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IdxNode] = PrefList,
	Obj1 = kesto_core_event_vnode:get_list(IdxNode),
	?debugVal(Obj1),
	?assert(lists:member(Event1, Obj1)),
	?assert(lists:member(Event2, Obj1)).

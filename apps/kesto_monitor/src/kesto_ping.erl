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

-module(kesto_ping).

-include_lib("eunit/include/eunit.hrl").

-include("kesto_monitor.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-compile(export_all).

demo1() ->
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
					   enabled=true,
					   ipv4=["127.0.0.1"], 
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
						  name="ping conf 1", 
						  description="ping conf 1 description", 
						  type=ping, 
						  facility_id="scope1", 
						  calendar_id="", 
						  cycle=60, 
						  times=2, 
						  interval=1, 
						  timeout=5000,  
						  enabled=true,
						  conf=#ping_conf{info_low=1000.0,
										   info_high=1.0,
										   warn_low=3000.0,
										   warn_high=51.0}},
	?debugVal(Conf1),
	ok = kesto_monitor_conf:put(Conf1).

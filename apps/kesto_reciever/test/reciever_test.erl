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

-module(reciever_test).

-include_lib("eunit/include/eunit.hrl").
-include("kesto_reciever.hrl").

-compile(export_all).

dep_apps() ->
	DelMe = "./EUnit-SASL.log",
	DataDir = "./EUnit-datadir",
	os:cmd("rm -rf " ++ DataDir),
	os:cmd("mkdir " ++ DataDir),
	KillDamnFilterProc = fun() ->
								 catch exit(whereis(riak_sysmon_filter), kill),
								 wait_until_dead(whereis(riak_sysmon_filter))
						 end,                                 
	Core_Settings = [{handoff_ip, "0.0.0.0"},
					 {handoff_port, 9183},
					 {ring_creation_size, 16},
					 {ring_state_dir, DataDir}],
	KV_Settings = [{storage_backend, riak_kv_multi_backend},
				   {multi_backend_default, memory},
				   {multi_backend, [
									{memory, 
									 riak_kv_memory_backend, 
									 [{max_memory, 128}, {ttl, 86400}]}]},
				   {vnode_vclocks, true},
				   {pb_ip, "0.0.0.0"},
				   {pb_port, 48087}, % arbitrary #
				   {map_js_vm_count, 4},
				   {reduce_js_vm_count, 3}],
	KestoCore_Settings = [{put_option, [{w, 2}, {dw, 1}, return_body]}],
	KestoReciever_Settings = [{syslog_port, 10514}],
	KestoMonitor_Settings = [{fping_path, "sudo /usr/local/sbin/fping"},
							 {fping6_path, "sudo /usr/local/sbin/fping6"}],
	[
	 fun(start) ->
			 net_kernel:start([reciever_test@localhost, shortnames]),
			 timer:sleep(50),
			 _ = application:stop(sasl),
			 _ = application:load(sasl),
			 put(old_sasl_l, app_helper:get_env(sasl, sasl_error_logger)),
			 ok = application:set_env(sasl, sasl_error_logger, {file, DelMe}),
			 ok = application:start(sasl),
			 %%error_logger:tty(false);
			 error_logger:tty(true);
		(stop) ->
			 ok = application:stop(sasl),
			 ok = application:set_env(sasl, sasl_error_logger, erase(old_sasl_l));
		(fullstop) ->
			 _ = application:stop(sasl)
	 end,
	 %% public_key and ssl are not needed here but started by others so
	 %% stop them when we're done.
	 crypto, public_key, ssl,
	 fun(start) ->
			 ok = application:start(riak_sysmon);
		(stop) ->
			 ok = application:stop(riak_sysmon),
			 KillDamnFilterProc();
		(fullstop) ->
			 _ = application:stop(riak_sysmon),
			 KillDamnFilterProc()
	 end,
	 webmachine,
	 os_mon,
	 lager,
	 fun(start) ->
			 _ = application:load(riak_core),
			 %% riak_core_handoff_listener uses {reusaddr, true}, but
			 %% sometimes we just restart too quickly and hit an
			 %% eaddrinuse when restarting riak_core?
			 timer:sleep(1000),
			 %% io:format(user, "DEBUGG: ~s\n", [os:cmd("netstat -na | egrep -vi 'stream|dgram'")]),
			 [begin
				  put({?MODULE,AppKey}, app_helper:get_env(riak_core, AppKey)),
				  ok = application:set_env(riak_core, AppKey, Val)
			  end || {AppKey, Val} <- Core_Settings],
			 ok = application:start(riak_core);
		(stop) ->
			 ok = application:stop(riak_core),
			 [ok = application:set_env(riak_core, AppKey, get({?MODULE, AppKey}))
					 || {AppKey, _Val} <- Core_Settings];
		(fullstop) ->
			 _ = application:stop(riak_core)
	 end,
	 riak_pipe,
	 luke,
	 erlang_js,
	 inets,
	 mochiweb,
	 fun(start) ->
			 _ = application:load(riak_kv),
			 [begin
				  put({?MODULE,AppKey}, app_helper:get_env(riak_kv, AppKey)),
				  ok = application:set_env(riak_kv, AppKey, Val)
			  end || {AppKey, Val} <- KV_Settings],
			 ok = application:start(riak_kv);
		(stop) ->
			 ok = application:stop(riak_kv),
			 net_kernel:stop(),
			 [ok = application:set_env(riak_kv, AppKey, get({?MODULE, AppKey}))
					 || {AppKey, _Val} <- KV_Settings];
		(fullstop) ->
			 _ = application:stop(riak_kv)
	 end,
	 fun(start) ->
			 _ = application:load(kesto_core),
			 [begin
				  put({?MODULE,AppKey}, app_helper:get_env(kesto_core, AppKey)),
				  ok = application:set_env(kesto_core, AppKey, Val)
			  end || {AppKey, Val} <- KestoCore_Settings],
			 ok = application:start(kesto_core);
		(stop) ->
			 ok = application:stop(kesto_core),
			 net_kernel:stop(),
			 [ok = application:set_env(kesto_core, AppKey, get({?MODULE, AppKey}))
					 || {AppKey, _Val} <- KestoCore_Settings];
		(fullstop) ->
			 _ = application:stop(kesto_core)
	 end,
	 fun(start) ->
			 _ = application:load(kesto_reciever),
			 [begin
				  put({?MODULE,AppKey}, app_helper:get_env(kesto_reciever, AppKey)),
				  ok = application:set_env(kesto_reciever, AppKey, Val)
			  end || {AppKey, Val} <- KestoReciever_Settings],
			 ok = application:start(kesto_reciever);
		(stop) ->
			 ok = application:stop(kesto_reciever),
			 net_kernel:stop(),
			 [ok = application:set_env(kesto_reciever, AppKey, get({?MODULE, AppKey}))
					 || {AppKey, _Val} <- KestoReciever_Settings];
		(fullstop) ->
			 _ = application:stop(kesto_reciever)
	 end,
	 fun(start) ->
			 _ = application:load(kesto_monitor),
			 [begin
				  put({?MODULE,AppKey}, app_helper:get_env(kesto_monitor, AppKey)),
				  ok = application:set_env(kesto_monitor, AppKey, Val)
			  end || {AppKey, Val} <- KestoMonitor_Settings],
			 ok = application:start(kesto_monitor);
		(stop) ->
			 ok = application:stop(kesto_monitor),
			 net_kernel:stop(),
			 [ok = application:set_env(kesto_monitor, AppKey, get({?MODULE, AppKey}))
					 || {AppKey, _Val} <- KestoMonitor_Settings];
		(fullstop) ->
			 _ = application:stop(kesto_monitor)
	 end].

do_dep_apps(fullstop) ->
	lists:map(fun(A) when is_atom(A) -> _ = application:stop(A);
				 (F)                 -> F(fullstop)
			  end, lists:reverse(dep_apps()));
do_dep_apps(StartStop) ->
	Apps = if StartStop == start -> dep_apps();
			  StartStop == stop  -> lists:reverse(dep_apps())
		   end,
	lists:map(fun(A) when is_atom(A) -> ok = application:StartStop(A);
				 (F)                 -> F(StartStop)
			  end, Apps).

prepare_runtime() ->
	fun() ->
			do_dep_apps(fullstop),
			timer:sleep(50),
			do_dep_apps(start),
			timer:sleep(50),
			riak_core:wait_for_service(riak_kv),
			riak_core:wait_for_service(riak_pipe),
			riak_core:wait_for_service(kesto_core),
			riak_core:wait_for_service(kesto_reciever),
			riak_core:wait_for_service(kesto_monitor),
			[foo1, foo2]
	end.

teardown_runtime() ->
	fun(_PrepareThingie) ->
			do_dep_apps(stop),
			timer:sleep(50)
	end.

reciever_test_() ->
	{foreach,
	 prepare_runtime(),
	 teardown_runtime(),
	 [
	  fun put_vnode_syslog/0,
	  fun put_vnode_other/0
	 ]
	}.

put_vnode_syslog() ->
	Data = #syslog{priority=158, 
				   timestamp={{2012,6,5},{11,30,14}}, 
				   host="Takahiros-MBA", 
				   tag="takahiro[2190]", 
				   body="test\n"},
	ok = kesto_reciever_syslog:put_vnode(Data),
	timer:sleep(100),
	Key = kesto_reciever_vnode:get_key(Data),
	{ok, C} = riak:local_client(),
	{ok, Obj} = C:get(<<"kesto_reciever">>, list_to_binary(Key)),
	?assertMatch(Data, riak_object:get_value(Obj)).

put_vnode_other() ->
	Data = #other{data={158, 
						{{2012,6,5},{11,30,14}}, 
						"Takahiros-MBA", 
						"takahiro[2190]", 
						"test\n"}},
	ok = kesto_reciever_syslog:put_vnode(Data),
	timer:sleep(100),
	Key = kesto_reciever_vnode:get_key(Data),
	{ok, C} = riak:local_client(),
	{ok, Obj} = C:get(<<"kesto_reciever">>, list_to_binary(Key)),
	?assertMatch(Data, riak_object:get_value(Obj)).

wait_until_dead(Pid) when is_pid(Pid) ->
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, _Obj, Info} ->
			Info
		after 10*1000 ->
			exit({timeout_waiting_for, Pid})
	end;
wait_until_dead(_) ->
	ok.

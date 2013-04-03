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
-module(kesto_sche).

-include("kesto_scheduler.hrl").
-include_lib("kesto_monitor/include/kesto_monitor.hrl").

-compile(export_all).

-compile([{parse_transform, lager_transform}]).

put1() ->
	Conf = #scheduler_conf{id="test1", 
							target_id="test1", 
							target_type=monitor, 
							type=simple, 
							cycle=10, 
							cron="", 
							enabled=true},
	ok = kesto_scheduler_conf:put(Conf).

del1() ->
	ok = kesto_scheduler_conf:delete("test1").

put2() ->
	Conf = #scheduler_conf{id="test2", 
							target_id="test2", 
							target_type=monitor, 
							type=simple, 
							cycle=15, 
							cron="", 
							enabled=true},
	ok = kesto_scheduler_conf:put(Conf).

del2() ->
	ok = kesto_scheduler_conf:delete("test2").

print_status_list([]) ->
	ok;
print_status_list([Status | T]) ->
	lager:info("~p", [Status]),
	print_status_list(T).
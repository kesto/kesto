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

-module(kesto_scheduler_task_fsm_sup).
-behavior(supervisor).

-export([start_check_fsm/1,
		 start_link/0]).
-export([init/1]).

start_check_fsm(Args) ->
	supervisor:start_child(?MODULE, Args).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	CheckFsm = {kesto_scheduler_task_fsm,
				{kesto_scheduler_task_fsm, start_link, []},
				temporary, 5000, worker, [kesto_scheduler_task_fsm]},
	
	{ok, {{simple_one_for_one, 10, 10}, [CheckFsm]}}.

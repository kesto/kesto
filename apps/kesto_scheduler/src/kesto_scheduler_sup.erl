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
%% KIND, either exprRecievrerSpecess or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc supervise the core kesto_core services

%% @private

-module(kesto_scheduler_sup).

-behaviour(supervisor).

-export([start_recieve/2]).
-export([start_link/0]).
-export([init/1]).

start_recieve(Node, Args) ->
	supervisor:start_child({?MODULE, Node}, Args).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	VMaster = {kesto_scheduler_vnode_master,
			   {riak_core_vnode_master, start_link, [kesto_scheduler_vnode]},
			   permanent, 5000, worker, [riak_core_vnode_master]},
	CheckFsm = {kesto_scheduler_check_fsm,
				{kesto_scheduler_check_fsm, start_link, []},
				permanent, 5000, worker, [kesto_scheduler_check_fsm]},
	TaskFsmSup = {kesto_scheduler_task_fsm_sup,
				  {kesto_scheduler_task_fsm_sup, start_link, []},
				  permanent, infinity, supervisor, [kesto_scheduler_task_fsm_sup]},
	
	% Build the process list...
	Processes = lists:flatten([
							   VMaster,
							   CheckFsm,
							   TaskFsmSup
							  ]),
	
	{ok, {{one_for_one, 10, 10}, Processes}}.

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

%% @doc supervise the core kesto_core services

%% @private

-module(kesto_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_Args) ->
    VMasterCore = {kesto_core_vnode_master,
                   {riak_core_vnode_master, start_link, [kesto_core_vnode]},
                   permanent, 5000, worker, [riak_core_vnode_master]},
    VMasterRepository = {kesto_core_repository_vnode_master,
                         {riak_core_vnode_master, start_link, [kesto_core_repository_vnode]},
                         permanent, 5000, worker, [riak_core_vnode_master]},
    VMasterEvent = {kesto_core_event_vnode_master,
                    {riak_core_vnode_master, start_link, [kesto_core_event_vnode]},
                     permanent, 5000, worker, [riak_core_vnode_master]},
    VMasterNotify = {kesto_core_notify_vnode_master,
                    {riak_core_vnode_master, start_link, [kesto_core_notify_vnode]},
                    permanent, 5000, worker, [riak_core_vnode_master]},
    VMasterMail = {kesto_core_mail_vnode_master,
                    {riak_core_vnode_master, start_link, [kesto_core_mail_vnode]},
                    permanent, 5000, worker, [riak_core_vnode_master]},
    VMasterCommand = {kesto_core_command_vnode_master,
                    {riak_core_vnode_master, start_link, [kesto_core_command_vnode]},
                    permanent, 5000, worker, [riak_core_vnode_master]},
	VMasterAuth = {kesto_core_auth_vnode_master,
				   {riak_core_vnode_master, start_link, [kesto_core_auth_vnode]},
					permanent, 5000, worker, [riak_core_vnode_master]},
	
    % Build the process list...
    Processes = lists:flatten([
        VMasterCore,
		VMasterRepository,
		VMasterEvent,
		VMasterNotify,
		VMasterMail,
		VMasterCommand,
		VMasterAuth
    ]),

    % Run the proesses...
    {ok, {{one_for_one, 10, 10}, Processes}}.

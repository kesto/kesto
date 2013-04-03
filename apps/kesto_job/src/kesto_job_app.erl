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

%% @author Takahiro Shidara <takahoge@gmail.com>
%% @doc kesto_poller's application module.

-module(kesto_job_app).

-behaviour(application).

-export([start/2, stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_StartType, _StartArgs) ->
	riak_core_util:start_app_deps(kesto_job),

    %% Append user-provided code paths
    case app_helper:get_env(kesto_job, add_paths) of
        List when is_list(List) ->
            ok = code:add_paths(List);
        _ ->
            ok
    end,

    %% Register our cluster_info app callback modules, with catch if
    %% the app is missing or packaging is broken.
    catch cluster_info:register_app(kesto_job_cinfo),

    %% Spin up supervisor
    case kesto_job_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(kesto_job, [
												{vnode_module, kesto_job_vnode}
											   ]),
			ok = riak_core_node_watcher:service_up(kesto_job, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak.
stop(_State) ->
    ok.

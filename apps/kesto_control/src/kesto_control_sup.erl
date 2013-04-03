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

-module(kesto_control_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Kesto_control_session={kesto_control_session,
						   {kesto_control_session, start_link, []},
						   permanent,
						   5000,
						   worker,
						   [kesto_control_session]},
	
	%% determine if riak_control is enabled or not
	case app_helper:get_env(kesto_control,enabled,false) of
		true ->
			Resources = [{kesto, kesto_gui}
						],
			Routes = lists:append([routes(E, M) || {E, M} <- Resources]),
			[webmachine_router:add_route(R) || R <- Routes],
			
			%% start kesto control
			{ok, { {one_for_one, 5, 10}, [Kesto_control_session] } };
		_ ->
			{ok, { {one_for_one, 5, 10}, [] } }
	end.

routes(Env, Module) ->
	case app_helper:get_env(kesto_control, Env, false) of
		true ->
			Module:routes();
		false ->
			[];
		_Other ->
			error_logger:warning_msg(
			  "Defaulting kesto_control appenv '~p' to 'false'."
				  " Found unknown \"~p\"", [Env, _Other])
	end.

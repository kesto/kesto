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

%% @doc RPC functions.
-module(kesto_control_rpc).

-export([perform_rpc_action/5]).

-include("kesto_control.hrl").

%% get the target node for the action
target_node(Req) ->
    list_to_existing_atom(dict:fetch(node,wrq:path_info(Req))).

%% remote to the target node, perform the action, and return
perform_rpc_action(Req,C,Module,Fun,Args) ->
    Node=target_node(Req),
    Result=case rpc:call(Node,Module,Fun,Args) of
               {badrpc,Error} -> {error,Error};
               Ok -> Ok
           end,
    kesto_control_formatting:action_result(Result,Req,C).

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

-module(kesto_routes).
-compile([export_all]).
-compile([{parse_transform, lager_transform}]).

%% helper, ensures all routes begin with /kesto
kesto_route (Rest) -> ["kesto"|Rest].

%% routes that query/act on a single node in the cluster
node_route (Route) -> kesto_route(["node",node|Route]).
node_route () -> kesto_route(["node",node]).

%% routes that query/act on the cluster
cluster_route (Route) -> kesto_route(["cluster"|Route]).
cluster_route () -> kesto_route(["cluster"]).

%% routes that query/act on partitions
ring_route (Route) -> kesto_route(["ring"|Route]).
ring_route () -> kesto_route(["ring"]).

%% routes that query/act on individual v-nodes
vnode_route (Route) -> kesto_route(["vnode",partition|Route]).
vnode_route () -> kesto_route(["vnode",partition]).


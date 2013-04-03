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
-module(kesto_reciever).
-include("kesto_reciever.hrl").

%% -export([start/0, stop/0]).

%% start() ->
%% 	application:start(kernel),
%% 	application:start(stdlib),
%% 	application:start(sasl),
%% 	application:start(public_key),
%% 	application:start(ssl),
%% 	application:start(riak_sysmon),
%% 	application:start(os_mon),
%% 	application:start(crypto),
%% 	application:start(runtime_tools),
%% 	application:start(erlang_js),
%% 	application:start(mochiweb),
%% 	application:start(webmachine),
%% 	application:start(luke),
%% 	application:start(basho_stats),
%% 	application:start(bitcask),
%% 	application:start(riak_core),
%% 	application:start(riak_pipe),
%% 	application:start(riak_kv),
%% 	application:start(riak_search),
%% 	application:start(cluster_info),
%% 	application:start(lager),
%% 	application:start(basho_metrics),
%% 	application:start(riak_control),
%% 	application:start(kesto_core),
%% 	application:start(kesto_reciever).
%% 
%% stop() ->
%%     ok.

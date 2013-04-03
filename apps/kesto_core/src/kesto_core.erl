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
%% @doc kesto_reciever's application module.

-module(kesto_core).
-include("kesto_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
		 ping/0,
		 priority_to_list/1,
		 datetime_to_list/1,
		 monitor_type_to_list/1
		]).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
	DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
	%PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, kesto_core),
	%[{IndexNode, _Type}] = PrefList,
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
	[IndexNode] = PrefList,
	%riak_core_vnode_master:sync_spawn_command(IndexNode, ping, kesto_core_vnode_master).
	riak_core_vnode_master:command(IndexNode, ping, kesto_core_vnode_master).

%% @spec priority_to_list(priority()) -> string().
%% @doc priority()を文字列に変換する。
-spec priority_to_list(priority()) -> string().
priority_to_list(Priority) ->
	erlang:atom_to_list(Priority).

%% @spec datetime_to_list(datetime()) -> string().
%% @doc datetime()を文字列に変換する。
-spec datetime_to_list(datetime()) -> string().
datetime_to_list(DateTime) ->
	{{Year, Mon, Day}, {Hour, Min, Sec}} = DateTime,
	Args = [Year, Mon, Day, Hour, Min, Sec],
	Str = io_lib:format("~B/~2.10.0B/~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", Args),
	lists:flatten(Str).

%% @spec monitor_type_to_list(monitor_type()) -> string().
%% @doc monitor_type()を文字列に変換する。
-spec monitor_type_to_list(monitor_type()) -> string().
monitor_type_to_list(MonitorType) ->
	erlang:atom_to_list(MonitorType).

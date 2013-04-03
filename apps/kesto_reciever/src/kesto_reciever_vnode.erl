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

-module(kesto_reciever_vnode).

-behaviour(riak_core_vnode).

-include("kesto_reciever.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-compile([{parse_transform, lager_transform}]).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-export([
         put/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([get_key/1]).
-endif.

-record(state, {partition}).

-define(MASTER, kesto_reciever_vnode_master).
-define(sync(PrefList, Command, Master),
        riak_core_vnode_master:sync_command(PrefList, Command, Master)).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition }}.

put(IdxNode, Data) ->
    riak_core_vnode_master:command(IdxNode,
                                   {put, Data},
                                   ?MASTER).

handle_command({put, Data}, _Sender, State) ->
	try riak_object:new(?KESTO_RECIEVER, list_to_binary(get_key(Data)), Data) of
		Object ->
			lager:debug("create riak object : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, [{w, 2}, {dw, 1}, return_body]) of
				ok ->
					check_monitor_vnode(Data);
				{error, Error} ->
					lager:error("put riak_object : ~p", [Error])
			end
	catch
		_:Error ->
			lager:error("new riak_object : ~p", [Error])
	end,
	{reply, ok, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

get_key(Data) when is_record(Data, syslog) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = Data#syslog.timestamp,
	Key = string:join(["syslog", 
					   integer_to_list(erlang:phash2(Data)),
					   integer_to_list(Year), 
					   integer_to_list(Month), 
					   integer_to_list(Day), 
					   integer_to_list(Hour), 
					   integer_to_list(Minute), 
					   integer_to_list(Second)], ";"),
	lager:debug("create key : ~s", [Key]),
	Key;

get_key(Data) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	Key = string:join(["other", 
					   integer_to_list(erlang:phash2(Data)),
					   integer_to_list(Year), 
					   integer_to_list(Month), 
					   integer_to_list(Day), 
					   integer_to_list(Hour), 
					   integer_to_list(Minute), 
					   integer_to_list(Second)], ";"),
	lager:debug("create key : ~s", [Key]),
	Key.

check_monitor_vnode(Data) when is_record(Data, syslog) ->
	DocIdx = riak_core_util:chash_key({<<"syslog">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_reciever),
	[IdxNode] = PrefList,
	kesto_monitor_vnode:check(IdxNode, Data);
check_monitor_vnode(_) ->
	ok.

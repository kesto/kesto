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
%% @doc syslog受信モジュール

-module(kesto_reciever_syslog).
-include("kesto_reciever.hrl").

-compile([{parse_transform, lager_transform}]).

-export([start_link/0, init/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([put_vnode/1]).
-endif.

-define(UDP_OPTIONS, [binary, {active, false}]).

start_link() ->
	{ok, proc_lib:spawn_link(?MODULE, init, [kesto_reciever_config:syslog_port()])}.

init(Port) ->
	lager:log(info, self(), "syslog受信用ポート[~p]をオープンしました。", [Port]),
	{ok, Socket} = gen_udp:open(Port, ?UDP_OPTIONS),
	listen_loop(Socket).

listen_loop(Socket) ->
	case gen_udp:recv(Socket, 0) of
		{ok, {_, _, Data}} ->
			lager:debug("syslogデータを受信しました。 : ~p", [Data]),
			try kesto_syslog:parse(binary_to_list(Data)) of
				{Priority, Timestamp, Host, Tag, Body} ->
					lager:debug("syslogデータをパースしました。 : priority = ~p, timestamp = ~p, host = ~p, tag = ~p, body = ~p", 
								[Priority, Timestamp, Host, Tag, Body]),
					SyslogData = #syslog{priority=Priority, 
										 timestamp=Timestamp, 
										 host=Host, 
										 tag=Tag, 
										 body=Body},
					put_vnode(SyslogData)
			catch
				_:Error ->
					lager:error("syslogデータのパースに失敗しました。 : ~p", [Error]),
					put_vnode(Data)
			end;
		Other ->
			lager:info("UDPデータの受信に失敗しました。 : ~p", [Other])
	end,
	listen_loop(Socket).

put_vnode(Data) when is_record(Data, syslog) ->
	DocIdx = riak_core_util:chash_key({<<"syslog">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_reciever),
	[IdxNode] = PrefList,
	kesto_reciever_vnode:put(IdxNode, Data);

put_vnode(Data) ->
	DocIdx = riak_core_util:chash_key({<<"other">>, term_to_binary(now())}),
	PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_reciever),
	[IdxNode] = PrefList,
	kesto_reciever_vnode:put(IdxNode, Data).

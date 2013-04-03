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
%% @doc Ping監視用モジュール

-module(kesto_monitor_ping).

-compile([{parse_transform, lager_transform}]).

-include("kesto_monitor.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-export([
		 run/1
		]).

%% @spec run(#monitor_conf{}) -> ok | {error, atom()} | {error, Error}
%% @doc Ping監視を実行する。
run(Conf) when is_record(Conf, monitor_conf) ->
	lager:info("ping監視開始 : ~p", [Conf]),
	
	{ok, TargetList} = kesto_core_repository:get_node_facility_id_list(
						 Conf#monitor_conf.facility_id, 
						 true),
	
	lager:info("ping監視対象ノード : ~p", [TargetList]),
	{Ipv4, Ipv6} = get_host_list(TargetList, dict:new(), dict:new()),
	
	case ping(Conf, get_ip_list(dict:to_list(Ipv4), []), 4) of
		{ok, Ipv4Result} ->
			check_result(Conf, Ipv4, Ipv4Result);
		{error, empty} ->
			lager:info("IPv4の監視対象が無い為、ping監視を実施しません。: ~p", [dict:to_list(Ipv4)])
	end,
	
	case ping(Conf, get_ip_list(dict:to_list(Ipv6), []), 6) of
		{ok, Ipv6Result} ->
			check_result(Conf, Ipv6, Ipv6Result);
		{error, empty} ->
			lager:info("IPv6の監視対象が無い為、ping監視を実施しません。: ~p", [dict:to_list(Ipv6)])
	end,
	
	lager:info("ping監視終了 : ~p", [Conf]).

%% @spec get_host_list([string()], dict(), dict()) -> [string()]
%% @doc ファシリティIDリストを元に、ファシリティIDをKey、IPアドレスのリストをValueとしてdictを生成する。
get_host_list([], Ipv4, Ipv6) ->
	{Ipv4, Ipv6};
get_host_list([FacilityId | T], Ipv4, Ipv6) ->
	case kesto_core_repository:get(FacilityId) of
		{ok, Node} ->
			NewIpv4 = case dict:is_key(Node#node.id, Ipv4) of
						  true ->
							  dict:append_list(Node#node.id, Node#node.ipv4, Ipv4);
						  false ->
							  dict:store(Node#node.id, Node#node.ipv4, Ipv4)
					  end,
			NewIpv6 = case dict:is_key(Node#node.id, Ipv6) of
						  true ->
							  dict:append_list(Node#node.id, Node#node.ipv6, Ipv6);
						  false ->
							  dict:store(Node#node.id, Node#node.ipv6, Ipv6)
					  end,
			get_host_list(T, NewIpv4, NewIpv6);
		{error, _} ->
			get_host_list(T, Ipv4, Ipv6)
	end.

%% @spec get_ip_list([string()], [string()]]) -> [string()]
%% @doc 受け取ったIPリストからIPアドレスの重複を取り除く。
get_ip_list([], IpList) ->
	IpList;
get_ip_list([{_K, V} | T], IpList) ->
	NewIpList = lists:umerge(lists:sort(V), lists:sort(IpList)),
	get_ip_list(T, NewIpList).

%% @spec ping(#monitor_conf{}, [string()], integer()]) -> {ok, string()} | {error, atom()}
%% @doc fpingを実行し、fpingの標準出力を結果として返す。
ping(Conf, IpList, Version) ->
	case erlang:length(IpList) of
		Len when Len > 0 ->
			lager:info("ping監視対象IP : ~p", [IpList]),
			Fping = case Version of
						4 ->
							kesto_monitor_config:fping_path();
						6 ->
							kesto_monitor_config:fping6_path()
					end,
			Count = "-C " ++ integer_to_list(Conf#monitor_conf.times),
			Interval = "-p " ++ integer_to_list(Conf#monitor_conf.interval),
			Timeout = "-T " ++ integer_to_list(Conf#monitor_conf.timeout),
			
			Cmd = string:join([Fping, Count, Interval, Timeout, "-q" | IpList], " "),
			lager:info("fpingコマンド : ~s", [Cmd]),
			
			Res = os:cmd(Cmd),
			lager:info("fping実行結果 : ~p", [Res]),
			{ok, Res};
		_ ->
			{error, empty}
	end.

%% @spec check_result(#monitor_conf{}, dict(), [string()]]) -> true | false
%% @doc fpingの結果を一行ずつに分割し一行ずつチェックする。
check_result(Conf, IpDict, Result) ->
	case erlang:length(Result) of
		Len when Len > 0 ->
			LineList = re:split(Result, "\\n", [{return, list}]),
			check_result_line(Conf, IpDict, LineList);
		_ ->
			lager:info("fpingの実行結果が空でした。 : ~p", [Conf])
	end.

%% @spec check_result_line([string()]) -> true | false
%% @doc fpingの結果を一行ずつチェックする。
check_result_line(_Conf, _IpDict, []) ->
	ok;
check_result_line(Conf, IpDict, [Line | T]) ->
	[Host|Result] = re:split(Line, "\\s+:\\s+|\\s", [{return, list}]),
	FacilityIdList = get_facility_id_list(IpDict, Host),
	case check_fping_result(Result) of
		true ->
			case calc_fping_result(Conf, Host, Result) of
				{ok, {Message, OrgMessage, Priority}} ->
					notify(Conf, FacilityIdList, Message, OrgMessage, Priority);
				{error, empty} ->
					lager:info("fpingの実行結果が空でした。 : ~p", [Conf]),
					ok
			end;
		false ->
			lager:info("fpingの実行結果が正常な実行結果ではありませんでした。 : ~p, ~p", [Conf, Result]),
			{Message, OrgMessage} = make_message(Result),
			notify(Conf, FacilityIdList, Message, OrgMessage, unknown)
	end,
	check_result_line(Conf, IpDict, T).

%% @spec get_facility_id_list(dict(), string()]) -> [string()]
%% @doc ファシリティIDをKey、IPアドレスのリストをValueとしたdictからIPが一致するファシリティIDリストを取得する。
get_facility_id_list(IpDict, Ip) ->
	lists:map(
	  fun({K, _V}) -> K end, 
	  lists:filter(
		fun({_K, V}) -> 
				lists:member(Ip, V)
		end, 
		dict:to_list(IpDict))).

notify(_, [], _, _, _) ->
	ok;
notify(Conf, [FacilityID | T], Message, OrgMessage, Priority) ->
	case kesto_core_repository:get(FacilityID) of
		{ok, Node} ->
			case is_list(Conf#monitor_conf.notify_id) of
				true ->
					NotifyInfo = #notify_info{id=Conf#monitor_conf.notify_id, 
											  priority=Priority, 
											  timestamp=calendar:local_time(), 
											  timestamp_raw=calendar:local_time(), 
											  monitor_type=ping, 
											  monitor_id=Conf#monitor_conf.id, 
											  facility_id=FacilityID, 
											  facility_name=Node#node.name, 
											  message=Message, 
											  org_message=OrgMessage},
					DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
					PrefList = riak_core_apl:get_apl(DocIdx, 1, kesto_core),
					[IdxNode] = PrefList,
					kesto_core_notify_vnode:execute(IdxNode, NotifyInfo);
				false ->
					Event = #event{id=kesto_core_event:get_id(),
								   priority=Priority, 
								   timestamp=calendar:local_time(), 
								   timestamp_raw=calendar:local_time(), 
								   monitor_type=ping, 
								   monitor_id=Conf#monitor_conf.id, 
								   facility_id=FacilityID, 
								   facility_name=Node#node.name, 
								   message=Message, 
								   org_message=OrgMessage,
								   check=false, 
								   comment=""},
					kesto_core_event:put(Event)
			end;
		{error, _Error} ->
			ok
	end,
	notify(Conf, T, Message, OrgMessage, Priority).

%% @spec check_fping_result(string()) -> true | false
%% @doc fpingの結果の一行の値が正常な応答かチェックする。
%%      fpingの実行結果
%%        応答あり						127.0.0.1 : 0.05 0.21
%%        応答なし						127.0.0.1 : - -
%%        その他(複数のIPから応答があり)		127.0.0.1 : duplicate for [0], xx bytes, x.xx ms
%%        その他(デフォルトGWが未設定)		127.0.0.1 : (空白)
%%      正常な実行結果を上記の「応答あり」及び「応答なし」とする。
check_fping_result(Result) ->
	CheckFun = fun(X) -> 
					   case re:run(X, "([0-9]+\\.[0-9]+|-)", [{capture, [1], list}]) of
						   {match, _} -> ok;
						   nomatch -> ng
					   end
			   end,
	case lists:member(ng, [CheckFun(X) || X <- Result]) of
		false -> true;
		true -> false
	end.

%% @spec calc_fping_result(string(), string()) -> {ok, {float(), float(), float()}} | {error, atom()}
%% @doc fpingの結果の一行から最小値、最大値、平均値を取得する。
calc_fping_result(Conf, Host, Result) ->
	case erlang:length(Result) of
		Len when Len > 0 ->
			NewResult = lists:map(
						  fun(X) -> erlang:list_to_float(X) end, 
						  lists:filter(
							fun(X) -> 
									case re:run(X, "([0-9]+\\.[0-9]+)", [{capture, [1], list}]) of
										{match, _} -> true;
										nomatch -> false
									end
							end, Result)),
			Num = erlang:length(NewResult),
			{Min, Max, Avg, Lost, Reach} = case Num == 0 of
											   true ->
												   {0, 0, 0, 100.0, 0.0};
											   false ->
												   {lists:min(NewResult), 
													lists:max(NewResult), 
													lists:sum(NewResult) / Num,
													(Len - Num) * 100 / Len, 
													Num * 100 / Len}
										   end,
			lager:info("fpingの実行結果の解析結果 : ~p", [{Min, Max, Avg, Lost, Reach, Num, Len}]),
			{Message, OrgMessage} = make_massage(Host, {Min, Max, Avg, Lost, Reach, Num, Len}),
			{ok, {Message, OrgMessage, get_priority(Conf#monitor_conf.conf, Lost, Avg)}};
		_ ->
			{error, empty}
	end.

get_priority(Conf, Lost, Avg) 
  when (Conf#ping_conf.info_high > Lost) and (Conf#ping_conf.info_low > Avg * 1000) ->
	info;
get_priority(Conf, Lost, Avg) 
  when (Conf#ping_conf.warn_high > Lost) and (Conf#ping_conf.warn_low > Avg * 1000) ->
	warning;
get_priority(_, _, _) ->
	error.

make_massage(Host, {Min, Max, Avg, Lost, _Reach, Num, Len}) ->
	TmpOrgMessage = 
		case Num == 0 of
			true ->
				Tmp = lists:map(fun(X) -> 
										lists:concat(["Reply from ", 
													  Host, 
													  " icmp_seq=",
													  erlang:integer_to_list(X),
													  " Destination Host Unreachable\n"])
								end, 
								lists:seq(1, Len)),
				lists:concat([Tmp, "\nPing statistics for ", Host, ":\n", 
							  "Packets: Sent = ", erlang:integer_to_list(Len), 
							  ", Received = ", erlang:integer_to_list(Num), 
							  ", Lost = ", erlang:integer_to_list(Len - Num), 
							  " (", lists:flatten(io_lib:format("~.1f", [Lost])), "% loss),"]);
			false ->
				lists:concat(["Ping statistics for ", Host, ":\n", 
							  "Packets: Sent = ", erlang:integer_to_list(Len), 
							  ", Received = ", erlang:integer_to_list(Num), 
							  ", Lost = ", erlang:integer_to_list(Len - Num), 
							  " (", lists:flatten(io_lib:format("~.1f", [Lost])), "% loss),\n",
							  "Approximate round trip times in milli-seconds:\n",
							  "\tMinimum = ", lists:flatten(io_lib:format("~.1f", [Min])),
							  "ms, Maximum = ", lists:flatten(io_lib:format("~.1f", [Max])),
							  "ms, Average = ", lists:flatten(io_lib:format("~.1f", [Avg])), "ms\n"])
		end,
	OrgMessage = lists:concat(["Pinging ", Host, " (", Host, ") .\n\n", TmpOrgMessage]),
	Message = lists:concat(["Packets: Sent = ", erlang:integer_to_list(Len), 
							", Received = ", erlang:integer_to_list(Num), 
							", Lost = ", erlang:integer_to_list(Len - Num), 
							" (", lists:flatten(io_lib:format("~.1f", [Lost])), "% loss)"]),
	lager:info("message = ~s, org_message = ~s", [Message, OrgMessage]),
	{Message, OrgMessage}.

make_message(Result) ->
	Message = "Failed to get a value.",
	OrgMessage = case erlang:length(Result) of
					 Len when Len > 0 ->
						 lists:concat(["Failed to get a value.\n\n", Result]);
					 _ ->
						 "Failed to get a value."
				 end,
	lager:info("message = ~s, org_message = ~s", [Message, OrgMessage]),
	{Message, OrgMessage}.
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
%% @doc 通知情報置き換えモジュール

-module(kesto_core_replace).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 replace/2
		]).

-define(RPLACEMENTS, [priority, 
					  timestamp_raw, 
					  timestamp, 
					  monitor_type, 
					  monitor_id, 
					  facility_id, 
					  facility_name, 
					  message, 
					  org_message]).
-define(REGEXP_PRIORITY, "\\$priority").
-define(REGEXP_TIMESTAMP, "\\$timestamp").
-define(REGEXP_TIMESTAMP_RAW, "\\$timestamp_raw").
-define(REGEXP_MONITOR_TYPE, "\\$monitor_type").
-define(REGEXP_MONITOR_ID, "\\$monitor_id").
-define(REGEXP_FACILITY_ID, "\\$facility_id").
-define(REGEXP_FACILITY_NAME, "\\$facility_name").
-define(REGEXP_MESSAGE, "\\$message").
-define(REGEXP_ORG_MESSAGE, "\\$org_message").

%% @spec replace(string(), notify_info()) -> string().
%% @doc 対象文字列内の置き換え対象文字列を通知情報の内容に置き換える。
-spec replace(string(), notify_info()) -> string().
replace(Text, NotifyInfo) ->
	replace_text(Text, NotifyInfo, ?RPLACEMENTS).

%% @spec replace_text(string(), notify_info(), [atom()]) -> string().
%% @doc 対象文字列内の置き換え対象文字列を通知情報の内容に置き換える。
-spec replace_text(string(), notify_info(), [atom()]) -> string().
replace_text(Target, _NotifyInfo, []) ->
	Target;
replace_text(Target, NotifyInfo, [Replacement | T]) ->
	case Replacement of
		priority ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_PRIORITY, NotifyInfo#notify_info.priority),
			replace_text(NewTarget, NotifyInfo, T);
		timestamp ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_TIMESTAMP, NotifyInfo#notify_info.timestamp),
			replace_text(NewTarget, NotifyInfo, T);
		timestamp_raw ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_TIMESTAMP_RAW, NotifyInfo#notify_info.timestamp_raw),
			replace_text(NewTarget, NotifyInfo, T);
		monitor_type ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_MONITOR_TYPE, NotifyInfo#notify_info.monitor_type),
			replace_text(NewTarget, NotifyInfo, T);
		monitor_id ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_MONITOR_ID, NotifyInfo#notify_info.monitor_id),
			replace_text(NewTarget, NotifyInfo, T);
		facility_id ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_FACILITY_ID, NotifyInfo#notify_info.facility_id),
			replace_text(NewTarget, NotifyInfo, T);
		facility_name -> 
			NewTarget = run_replace(Target, Replacement, ?REGEXP_FACILITY_NAME, NotifyInfo#notify_info.facility_name),
			replace_text(NewTarget, NotifyInfo, T);
		message ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_MESSAGE, NotifyInfo#notify_info.message),
			replace_text(NewTarget, NotifyInfo, T);
		org_message ->
			NewTarget = run_replace(Target, Replacement, ?REGEXP_ORG_MESSAGE, NotifyInfo#notify_info.org_message),
			replace_text(NewTarget, NotifyInfo, T)
	end.

%% @spec run_replace(string(), replacement(), string(), string()) -> string().
%% @doc 対象文字列内の置き換え対象文字列を通知情報の内容に置き換える。
-spec run_replace(string(), replacement(), string(), string()) -> string().
run_replace(Target, Replacement, Regexp, Replace) ->
	case re:compile(Regexp) of
		{ok, MP} ->
			run_replace_text(Target, Replacement, MP, Replace);
		{error, _Error} ->
			lager:error("reオブジェクトのコンパイルに失敗しました。 : ~p", [_Error]),
			Target
	end.

%% @spec run_replace_text(string(), replacement(), term(), string()) -> string().
%% @doc 対象文字列内の置き換え対象文字列を通知情報の内容に置き換える。
-spec run_replace_text(string(), replacement(), term(), string()) -> string().
run_replace_text(Target, Replacement, MP, Replace) ->
	case re:run(Target, MP) of
		{match, _Captured} ->
			NewReplace = case is_list(Replace) of
							 true ->
								 Replace;
							 false ->
								 case Replacement of
									 priority ->
										 kesto_core:priority_to_list(Replace);
									 timestamp ->
										 kesto_core:datetime_to_list(Replace);
									 timestamp_raw ->
										 kesto_core:datetime_to_list(Replace);
									 monitor_type ->
										 kesto_core:monitor_type_to_list(Replace)
								 end
						 end,
			NewTarget = re:replace(erlang:list_to_binary(Target), MP, NewReplace),
			run_replace_text(binList_to_list(NewTarget), Replacement, MP, NewReplace);
		nomatch ->
			Target
	end.

%% @spec binList_to_list([list() | binary()]) -> string().
%% @doc バイナリリストを文字列に変換する。
-spec binList_to_list([list() | binary()]) -> string().
binList_to_list([]) ->
	[];
binList_to_list(Lists) when is_binary(Lists) -> 
	binary_to_list(Lists);
binList_to_list(Lists=[Head|Tails]) when is_list(Lists) ->
	case is_binary(Head) of
		true ->
			HeadStr = binary_to_list(Head),
			HeadStr ++ binList_to_list(Tails);
		false ->
			Head ++ binList_to_list(Tails)
	end.
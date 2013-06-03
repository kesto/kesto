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

%% @doc Convenience functions for setting up the HTTP interface
%%      of Riak.  This module loads parameters from the application
%%      environment:
%%
%%<dl><dt> raw_name
%%</dt><dd>   the base path under which the kesto_api_wm_raw
%%            should be exposed; defaulted to "raw"
%%</dd></dl>

-module(kesto_api_web).

-compile([{parse_transform, lager_transform}]).

-export([dispatch_table/0]).
-include("kesto_api_wm_raw.hrl").

dispatch_table() ->
	lists:append(raw_dispatch(),
				 [
				  {["ping"], kesto_api_wm_ping, []}
				 ]).

raw_dispatch() ->
	case app_helper:get_env(kesto_api, raw_name) of
		undefined -> raw_dispatch("kesto");
		Name -> lists:append(raw_dispatch(Name), raw_dispatch("kesto"))
	end.

raw_dispatch(Name) ->
	Props1 = [{api_version, 1}|raw_props(Name)],
	[
	 {["users", user],
	  kesto_api_wm_auth, Props1}
	].

is_post(Req) ->
	lager:info("~p", [Req]),
	wrq:method(Req) == 'POST'.

is_props(Req) ->
	lager:info("~p", [Req]),
	(wrq:get_qs_value(?Q_PROPS, Req) /= ?Q_FALSE) andalso (not is_keylist(Req)).

is_keylist(Req) ->
	lager:info("~p", [Req]),
	X = wrq:get_qs_value(?Q_KEYS, Req),
	(X == ?Q_STREAM) orelse (X == ?Q_TRUE).

raw_props(Prefix) ->
	lager:info("~p", [Prefix]),
	[{prefix, Prefix}].

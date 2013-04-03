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

-module(kesto_core_wm_repository).

-compile([{parse_transform, lager_transform}]).

%% webmachine resource exports
-export([
		 init/1,
		 allowed_methods/2,
		 content_types_provided/2,
		 content_types_accepted/2,
		 process_post/2
		]).

%% @type context() = term()
-record(ctx, {api_version,  %% integer() - Determine which version of the API to use.
			  repository,   %% string() - Bucket name (from uri)
			  key,          %% string() - Key (from uri)
			  prefix,       %% string() - prefix for resource uris
			  doc,          %% {ok, riak_object()}|{error, term()} - the object found
			  method        %% atom() - HTTP method for the request
			 }).

-include_lib("webmachine/include/webmachine.hrl").
-include("kesto_core_wm_raw.hrl").

%% @spec init(proplist()) -> {ok, context()}
%% @doc Initialize this resource.  This function extracts the
%%      'prefix' and 'riak' properties from the dispatch args.
init(Props) ->
	{ok, #ctx{api_version=proplists:get_value(api_version, Props),
			  prefix=proplists:get_value(prefix, Props)}}.

%% @spec allowed_methods(reqdata(), context()) ->
%%          {[method()], reqdata(), context()}
%% @doc Get the list of methods this resource supports.
allowed_methods(ReqData, Ctx) ->
	{['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

process_post(ReqData, Ctx) ->
%%     [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
%%     {struct, Doc} = mochijson2:decode(JsonDoc),
%%     NewDoc = ece_db:create(Ctx#ctx.db, {Doc}),
%%     ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
%%     {true, ReqData2, Ctx}.
	ok.

to_json(ReqData, Ctx) ->
%%     case wrq:path_info(id, ReqData) of
%%         undefined ->
%%             All = ece_db:all(Ctx#ctx.db),
%%             {All, ReqData, Ctx};
%%         ID ->
%%             JsonDoc = ece_db:find(Ctx#ctx.db, ID),
%%             {JsonDoc, ReqData, Ctx}
%%     end.
	ok.

from_json(ReqData, Ctx) ->
%%     case wrq:path_info(id, ReqData) of
%%         undefined ->
%%             {false, ReqData, Ctx};
%%         ID ->
%%             JsonDoc = wrq:req_body(ReqData),
%%             {struct, Doc} = mochijson2:decode(JsonDoc),
%%             NewDoc = ece_db:update(Ctx#ctx.db, ID, Doc),
%%             ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
%%             {true, ReqData2, Ctx}
%%     end.
	ok.
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

-module(kesto_gui).

-compile([{parse_transform, lager_transform}]).

-export([routes/0,
		 init/1,
		 content_types_provided/2,
		 resource_exists/2,
		 previously_existed/2,
		 moved_permanently/2,
		 to_resource/2,
		 is_authorized/2,
		 service_available/2
		]).

%% riak_control and webmachine dependencies
-include("kesto_control.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% mappings to the various content types supported for this resource
-define(CONTENT_TYPES,[{"text/css", to_resource},
					   {"text/html",to_resource},
					   {"text/plain",to_resource},
					   {"text/javascript",to_resource}
					  ]).

%% defines the webmachine routes this module handles
routes() ->
	[{kesto_routes:kesto_route([]),?MODULE,index},
	 {kesto_routes:kesto_route(["ui",'*']),?MODULE,undefined},
	 {kesto_routes:kesto_route(["ui","index.html"]),?MODULE,oldindex}
	].

%% entry-point for the resource from webmachine
init(Resource) -> 
	%% 	{ok,Resource}.
	{{trace, "/tmp"}, Resource}.

%% redirect to the index page if no file given
moved_permanently(Req,oldindex) ->
	{{true,"kesto"},Req,index};
moved_permanently(Req,Ctx) ->
	{false,Req,Ctx}.

%% the index file isn't here
previously_existed(Req,oldindex) -> 
	{true,Req,oldindex};
previously_existed(Req,Ctx) -> 
	{false,Req,Ctx}.

%% a resource other than the index is here
resource_exists (Req,oldindex) -> 
	{false,Req,oldindex};
resource_exists (Req,Ctx) -> 
	{true,Req,Ctx}.

%% redirect to SSL port if using HTTP
service_available(Req,Ctx) ->
	kesto_control_security:scheme_is_available(Req,Ctx).

%% validate username and password
is_authorized(Req,Ctx) ->
	{true,Req,Ctx}.

%% return the list of available content types for webmachine
content_types_provided(Req,Ctx=index) ->
	{[{"text/html", to_resource}],Req, Ctx};
content_types_provided(Req,Ctx) ->
	Index = file_path(Req),
	MimeType = webmachine_util:guess_mime(Index),
	{[{MimeType, to_resource}],Req, Ctx}.

%% return file path
file_path(Req) ->
	Path=wrq:path_tokens(Req),
	filename:join([kesto_control:priv_dir(),"kesto"] ++ Path).

%% loads a resource file from disk and returns it
get_file(Req) ->
	Index = file_path(Req),
	{ok,Source}=file:read_file(Index),
	Source.

%% respond to an index request
to_resource(Req,Ctx=index) ->
	{ok, Content} = kesto_index_dtl:render([]),
	{Content, Req, Ctx};

%% respond to a file request
to_resource(Req,Ctx) ->
	{get_file(Req),Req,Ctx}.

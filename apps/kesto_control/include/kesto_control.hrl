%% Copyright (c) 2012 Conversion Co., Ltd. All Rights Reserved.
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

-include_lib("kesto_core/include/kesto_core_type.hrl").

-type version()       :: integer().
-type index()         :: binary().
-type status()        :: valid | invalid | down | leaving | incompatible.
-type home()          :: primary | fallback | undefined.
-type service()       :: atom().
-type services()      :: [{service(),home()}].
-type owner()         :: atom().
-type vnode()         :: {{atom(),atom()},atom()}. % {{Idx,Node},Status}
-type handoff()       :: {{atom(),atom()},atom()}. % {{Mod,Idx},TargetNode}
-type online()        :: boolean().

-record(partition_info,
        { index       :: index(),
          partition   :: integer(),
          owner       :: owner(),
          vnodes      :: services(),
          handoffs    :: [handoff()]
        }).

-record(member_info,
        { node        :: atom(),
          status      :: status(),
          reachable   :: boolean(),
          vnodes      :: [vnode()],
          handoffs    :: [handoff()],
          ring_pct    :: float(),
          pending_pct :: float(),
          mem_total   :: integer(),
          mem_used    :: integer(),
          mem_erlang  :: integer()
        }).

%% These two should always match, in terms of webmachine dispatcher
%% logic, and KESTO_BASE_PATH should always end with a /
-define(KESTO_BASE_PATH, "/kesto/").
-define(KESTO_BASE_ROUTE, ["kesto"]).

%% Value for WWW-Authenticate header
-define(KESTO_AUTH_HEAD, "Basic realm=kesto").

%% Names of HTTP header fields
-define(HEAD_CTYPE,           "Content-Type").

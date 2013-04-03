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

-ifndef(KESTO_CORE_TYPE).

-define(KESTO_CORE_TYPE, true).

%% 日時
-type(datetime() :: {{integer(), integer(), integer()},{integer(), integer(), integer()}}).

%% 時刻
-type(time() :: {integer(), integer(), integer()}).

%% 重要度
-type(priority() :: info | warning | error | unknown).

%% ファシリティID
-type(facility_id() :: string()).

%% システムID
-type(system_id() :: string()).

%% ロールID
-type(roll_id() :: string()).

%% ユーザID
-type(user_id() :: string()).

%% 機能ID
-type(function_id() :: job | monitor | repository | notify | atom()).

%% 権限
-type(auth() :: none | read | write | exec).

-endif.
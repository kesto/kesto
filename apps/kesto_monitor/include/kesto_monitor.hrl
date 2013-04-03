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

-define(KESTO_MONITOR_CONF_BUCKET, <<"kesto_monitor_conf">>).

%% 監視項目
-record(monitor_conf, {id, 
					   name, 
					   description, 
					   type, 
					   facility_id, 
					   calendar_id, 
					   cycle, 
					   times, 
					   interval, 
					   timeout, 
					   notify_id, 
					   enabled, 
					   conf}).

%% 監視項目(syslog)
-record(syslog_conf, {order_no,
					  case_sensitive,
					  pattern,
					  mode,
					  priority,
					  message}).

%% 監視項目(ping)
-record(ping_conf, {info_low,
					info_high,
					warn_low,
					warn_high}).

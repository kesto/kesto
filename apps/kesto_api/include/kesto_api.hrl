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

-define(KESTO_CORE_REPOSITORY_BUCKET, <<"kesto_repository">>).
-define(KESTO_CORE_EVENT_BUCKET, <<"kesto_event">>).
-define(KESTO_CORE_NOTIFY_BUCKET, <<"kesto_notify">>).
-define(KESTO_CORE_NOTIFY_HISTORY_BUCKET, <<"kesto_notify_history">>).
-define(KESTO_CORE_MAIL_TEMPLATE_BUCKET, <<"kesto_mail_template">>).
-define(KESTO_CORE_AUTH_BUCKET, <<"kesto_auth">>).
-define(KESTO_CORE_AUTH_SESSION_BUCKET, <<"kesto_auth_session">>).

%% ノード
-record(node, {id :: facility_id(), 
			   name :: string(), 
			   description :: string(), 
			   type :: linux | windows,
			   enabled :: true | false, 
			   ipv4 :: term(), 
			   ipv6 :: term(), 
			   ssh_port :: integer(), 
			   ssh_user :: string(), 
			   ssh_password :: string(), 
			   hostname :: string(), 
			   create_timestamp :: datetime(),
			   update_timestamp :: datetime(), 
			   snmp :: term(),
			   device :: term()}).
-type(repository_node() :: #node{}).

%% スコープ
-record(scope, {id :: facility_id(), 
				name :: string(), 
				description :: string(), 
				enabled :: true | false, 
				children :: [string()],
				create_timestamp :: datetime(),
				update_timestamp :: datetime()}).
-type(repository_scope() :: #scope{}).

%% 監視種別 
-type(monitor_type() :: syslog | ping).
%% 通知置き換え文字 
-type(replacement() :: priority | timestamp | timestamp_raw | monitor_type | monitor_id | facility_id | facility_name | message | org_message).

%% イベント
-record(event, {id :: string(), 
				priority :: priority(), 
				timestamp :: datetime(), 
				timestamp_raw :: datetime(), 
				monitor_type :: monitor_type(), 
				monitor_id :: string(), 
				facility_id :: facility_id(), 
				facility_name :: string(), 
				message :: string(), 
				org_message :: string(),
				check :: true | false, 
				comment :: string()}).
-type(event() :: #event{}).

%% 通知定義
-record(notify_conf, {id :: string(), 
					  description :: string(), 
					  type :: event | mail | test,
					  initial_count :: integer(),
					  renotify_type :: all | period | count | ignore,
					  renotify_value :: integer(),
					  create_timestamp :: datetime(),
					  update_timestamp :: datetime(), 
					  enabled :: true | false, 
					  conf :: notify_mail_conf() | notify_event_conf() | notify_command_conf}).
-type(notify_conf() :: #notify_conf{}).

%% 通知情報
-record(notify_info, {id :: [string()], 
					  priority :: priority(), 
					  timestamp :: datetime(), 
					  timestamp_raw :: datetime(), 
					  monitor_type :: monitor_type(), 
					  monitor_id :: string(), 
					  facility_id :: facility_id(), 
					  facility_name :: string(), 
					  message :: string(), 
					  org_message :: string()}).
-type(notify_info() :: #notify_info{}).

%% 通知履歴
-record(notify_history, {id :: string(), 
						 monitor_type :: monitor_type(), 
						 monitor_id :: string(), 
						 facility_id :: facility_id(),
						 priority :: priority(), 
						 timestamp :: datetime(),
						 initial_notify :: true | false,
						 counter :: integer()}).
-type(notify_history() :: #notify_history{}).

%% メール通知定義
-record(notify_mail_conf, {mail_template_id :: string(),
						   info_enabled :: true | false, 
						   info_address :: [string()], 
						   warning_enabled :: true | false, 
						   warning_address :: [string()], 
						   error_enabled :: true | false, 
						   error_address :: [string()], 
						   unknown_enabled :: true | false, 
						   unknown_address :: [string()]}).
-type(notify_mail_conf() :: #notify_mail_conf{}).

%% イベント通知定義
-record(notify_event_conf, {info_enabled :: true | false, 
							info_check ::  true | false, 
							warning_enabled :: true | false, 
							warning_check ::  true | false, 
							error_enabled :: true | false, 
							error_check ::  true | false, 
							unknown_enabled :: true | false, 
							unknown_check ::  true | false}).
-type(notify_event_conf() :: #notify_event_conf{}).

%% コマンド通知定義
-record(notify_command_conf, {info_enabled :: true | false, 
							  info_user :: string(), 
							  info_password :: string(), 
							  info_cmd :: string(), 
							  info_cmd_timeout :: integer(), 
							  warning_enabled :: true | false, 
							  warning_user :: string(), 
							  warning_password :: string(), 
							  warning_cmd :: string(), 
							  warning_cmd_timeout :: integer(), 
							  error_enabled :: true | false, 
							  error_user :: string(), 
							  error_password :: string(), 
							  error_cmd :: string(), 
							  error_cmd_timeout :: integer(), 
							  unknown_enabled :: true | false, 
							  unknown_user :: string(), 
							  unknown_password :: string(), 
							  unknown_cmd :: string(),
							  unknown_cmd_timeout ::  integer()}).
-type(notify_command_conf() :: #notify_command_conf{}).

%% メールテンプレート定義
-record(mail_template_conf, {id :: string(), 
							 description :: string(), 
							 subject :: string(),
							 body :: string()}).
-type(mail_template_conf() :: #mail_template_conf{}).

%% システム定義
-record(system, {id :: system_id(), 
				 description :: string(),
				 create_timestamp :: datetime(),
				 update_timestamp :: datetime()}).
-type(system() :: #system{}).

%% ロール定義
-record(roll, {id :: roll_id(), 
			   description :: string(), 
			   system_id :: system_id(),
			   auth :: [{function_id(), [auth()]}],
			   create_timestamp :: datetime(),
			   update_timestamp :: datetime()}).
-type(roll() :: #roll{}).

%% ユーザ定義
-record(user, {id :: user_id(), 
			   password :: string(), 
			   description :: string(), 
			   system_id :: system_id(),
			   mail :: string(), 
			   roll :: [roll_id()],
			   create_timestamp :: datetime(),
			   update_timestamp :: datetime()}).
-type(user() :: #user{}).

%% セッション定義
-record(auth_session, {id :: user_id(), 
					   session_id :: string(),
					   expiration_datetime :: datetime(),
					   user_agent :: string(), 
					   ip_address :: string()}).
-type(auth_session() :: #auth_session{}).

%% 認証定義種別
-type(auth_type() :: user | roll | system).
-type(auth_conf() :: user() | roll() | system()).
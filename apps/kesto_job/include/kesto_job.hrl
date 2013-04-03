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

-define(KESTO_CORE_JOB_CONF_BUCKET, <<"kesto_job_conf">>).
-define(KESTO_CORE_JOB_SESSION_BUCKET, <<"kesto_job_session">>).
-define(KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, <<"kesto_job_session_operation">>).
-define(KESTO_CORE_JOB_SCHEDULE_BUCKET, <<"kesto_job_schedule">>).

-define(RETRY_SESSION_LOCK_MAX, 5).

%% ジョブユニット種別
-type(job_type() :: group | job | net | judgment).
%% ジョブ状態
-type(job_state() :: wait | 'end' | running | suspended | reserved | skip | stopping | stoped | changed | unexecuted | error).
%% ジョブ状態
-type(job_end_state() :: info | warning | error | unexecuted).
%% ジョブ終了値
-type(job_end_value() :: integer()).
%% セッションID
-type(session_id() :: string()).
%% グループID
-type(group_id() :: string()).
%% ジョブID
-type(job_id() :: string()).
%% ジョブ操作引数
-type(job_operation_args() :: {session_id()} | {session_id(), group_id(), job_id()} | {session_id(), group_id(), job_id(), facility_id()}).

%% ジョブオペレーションID
-type(operation_id() :: string()).
%% ジョブオペレーション
-type(job_operation() :: cancel | skip | cancel_skip | suspend | cancel_suspended | reserve | cancel_reserved | stop | start | restart | change | force_stop | check).
%% ジョブオペレーション状態
-type(job_operation_state() :: wait | 'end' | running | error).
%% セッション操作メソッド
-type(job_operate_method() :: job_operation() | terminate | next).

%% セッションジョブ及びセッションノードの終了状態が終了可能ならtrueを返す。
-define(IS_END_KESTO_JOB(State), (State == 'end') or (State == changed) or (State == unexecuted)).
%% セッションジョブ及びセッションノードの終了状態が稼働中ならtrueを返す。
-define(IS_RUNNING_KESTO_JOB(State), (State == running) or (State == stopping)).

%% ジョブ定義
-record(job_conf, {id :: job_id(), 
				   name :: string(), 
				   description :: string(), 
				   type :: job_type(), 
				   recovery :: true | false, 
				   group_id :: group_id(), 
				   parent_id :: {group_id(), job_id()}, 
				   child_id :: [{group_id(), job_id()}], 
				   dependent_id :: {group_id(), job_id()}, 
				   previous_id :: [{group_id(), job_id()}], 
				   next_id :: [{group_id(), job_id()}], 
				   notify_id :: string(), 
				   info_end_value_from :: integer(), 
				   info_end_value_to :: integer(), 
				   warning_end_value_from :: integer(), 
				   warning_end_value_to :: integer(), 
				   judgement :: job_conf_judgement(), 
				   control :: job_conf_control(),
				   command :: job_conf_command(), 
				   delay :: job_conf_delay(), 
				   create_timestamp :: datetime(),
				   update_timestamp :: datetime()}).
-type(job_conf() :: #job_conf{}).

%% ジョブ定義(制御)
-record(job_conf_control, {calendar :: true | false, 
						   calendar_id :: string(), 
						   calendar_end_state :: integer(), 
						   control_skip :: true | false, 
						   control_reserve :: true | false, 
						   control_end_state :: job_end_state(), 
						   unmatch :: true | false, 
						   unmatch_end_status :: integer()}).
-type(job_conf_control() :: #job_conf_control{}).

%% ジョブ定義(コマンド)
-record(job_conf_command, {cmd_facility_id :: string(), 
						   cmd_start_command :: string(), 
						   cmd_stop_command :: string(), 
						   cmd_effective_user :: string(), 
						   cmd_error_end_flg :: true | false, 
						   cmd_error_end_value :: integer()}).
-type(job_conf_command() :: #job_conf_command{}).

%% ジョブ定義(遅延)
-record(job_conf_delay, {start_delay :: true | false, 
						 start_delay_session :: true | false, 
						 start_delay_session_value :: integer(), 
						 start_delay_time :: true | false, 
						 start_delay_time_value :: time(), 
						 start_delay_condition_type :: 'and' | 'or',
						 start_delay_notify :: true | false, 
						 start_delay_notify_priority :: priority(), 
						 start_delay_operation :: true | false, 
						 start_delay_operation_type :: integer(), 
						 start_delay_operation_end_status :: integer(), 
						 end_delay :: true | false, 
						 end_delay_session :: true | false, 
						 end_delay_session_value :: integer(), 
						 end_delay_job :: true | false, 
						 end_delay_job_value :: integer(), 
						 end_delay_time :: true | false, 
						 end_delay_time_value :: time(), 
						 end_delay_condition_type :: 'and' | 'or',
						 end_delay_notify :: true | false, 
						 end_delay_notify_priority :: priority(), 
						 end_delay_operation :: true | false, 
						 end_delay_operation_type :: integer(), 
						 end_delay_operation_end_status :: integer()}).
-type(job_conf_delay() :: #job_conf_delay{}).

%% ジョブ定義(判断)
-record(job_conf_judgement, {type :: end_state | parameter,
							 value :: job_end_state() | integer() | string()}).
-type(job_conf_judgement() :: #job_conf_judgement{}).

%% セッション情報
-record(job_session, {id :: session_id(),
					  job_id :: job_id(),
					  group_id :: group_id(), 
					  state=wait :: job_state(), 
					  trigger_type :: integer(),
					  trigger_value :: string(),
					  start_timestamp :: datetime(),
					  end_timestamp :: datetime()}).
-type(job_session() :: #job_session{}).

%% セッションジョブ情報
-record(job_session_job, {id :: session_id(), 
						  job_id :: job_id(), 
						  group_id :: group_id(), 
						  conf :: job_conf(),
						  state=wait :: job_state(), 
						  end_value :: integer(),
						  end_state :: job_end_state(), 
						  child_end_state=[] :: [job_end_state()],
						  child_end_value=[] :: [job_end_value()], 
						  dependent_end_state :: job_end_state(), 
				   		  previous_end_state :: job_end_state(), 
						  start_timestamp :: datetime(),
						  end_timestamp :: datetime()}).
-type(job_session_job() :: #job_session_job{}).

%% セッションノード情報
-record(job_session_node, {id :: session_id(), 
						   job_id :: job_id(), 
						   group_id :: group_id(), 
						   facility :: node(), 
						   state=wait :: job_state(), 
						   cmd_state=wait :: job_state(), 
						   end_value :: integer(),
						   message :: string(), 
						   start_timestamp :: datetime(),
						   end_timestamp :: datetime()}).
-type(job_session_node() :: #job_session_node{}).
-type(job_session_info() :: job_session() | job_session_job() | job_session_node()).

%% セッションオペレーション情報
-record(job_session_operation, {id :: operation_id(),
								session_id :: session_id(), 
								job_id :: job_id(), 
								group_id :: group_id(), 
								facility_id :: facility_id(), 
								operation :: job_operation(), 
								state :: job_operation_state(),
								end_value :: job_end_value() | job_end_state(),
								message :: string(), 
								create_timestamp :: datetime(),
								start_timestamp :: datetime(),
								end_timestamp :: datetime()}).
-type(job_session_operation() :: #job_session_operation{}).

%% セッションFSM状態
-record(job_session_state, {from :: term(), 
							module :: term(),
							operation_id :: operation_id(),
							operation :: job_operation(),
							operation_end_value :: job_end_value() | job_end_state(),
							session_info :: job_session_info(),
							command :: job_conf_command()}).
-type(job_session_state() :: #job_session_state{}).

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
%% @doc メール管理モジュール

-module(kesto_core_mail).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 notify/2
		]).

%% @spec notify(string(), notify_info()) -> ok | {error, atom()} | {error, term()}.
%% @doc イベントをRiakに登録する。
-spec notify(string(), notify_info()) -> ok | {error, atom()} | {error, term()}.
notify(NotifyID, NotifyInfo) ->
	lager:info("~p", [{NotifyID, NotifyInfo}]),
	
	Conf = case kesto_core_notify:get(NotifyID) of
			   {ok, Obj1} -> Obj1;
			   {error, _} -> notfound
		   end,
	case is_record(Conf, notify_conf) of
		true ->
			case is_record(Conf#notify_conf.conf, notify_mail_conf) of
				true ->
					MailConf = Conf#notify_conf.conf,
					MailTemplateID = MailConf#notify_mail_conf.mail_template_id,
					MailTemplateConf = case kesto_core_mail_template:get(MailTemplateID) of
										   {ok, Obj2} -> Obj2;
										   {error, _} -> notfound
									   end,
					case is_record(MailTemplateConf, mail_template_conf) of
						true ->
							case send_mail(NotifyInfo, MailConf, MailTemplateConf) of
								{ok, _} -> ok;
								{error, _Reason} -> {error, _Reason}
							end;
						false ->
							lager:error("メールテンプレート定義[~s]が存在しないため、メール通知を行いません。", [MailTemplateID]),
							{error, notfound}
					end;
				false ->
					lager:error("通知定義[~s]にメール通知定義が含まれていないため、メール通知を行いません。", [NotifyID]),
					{error, notfound}
			end;
		false ->
			lager:error("通知定義[~s]が存在しないため、メール通知を行いません。", [NotifyID]),
			{error, notfound}
	end.

%% @spec send_mail(notify_info(), notify_mail_conf(), mail_template_conf()) -> ok.
%% @doc 通知情報、メール通知定義、メールテンプレート定義を元にメールを送信する。
-spec send_mail(notify_info(), notify_mail_conf(), mail_template_conf()) -> ok.
send_mail(NotifyInfo, MailConf, MailTemplateConf) ->
	case check_priority(NotifyInfo, MailConf) of
		{true, Address} ->
			Subject = get_subject(NotifyInfo, MailTemplateConf),
			Body = get_body(NotifyInfo, MailTemplateConf),
			Mail = lists:concat(["Subject: ", Subject, "\r\n",
								 "From: ", kesto_core_config:mail_from(), "\r\n",
								 "To: ", string:join(Address, ", "), "\r\n\r\n",
								 Body]),
			lager:info("メール通知を実施。~s", [Mail]),
			gen_smtp_client:send({kesto_core_config:mail_from(), 
								  Address, 
								  Mail}, 
								 kesto_core_config:smtp_option());
		false ->
			lager:debug("重要度別通知フラグにチェックが無いため、メール通知を行いません。: ~p, ~p", [NotifyInfo, MailConf]),
			{error, nocheck}
	end.

%% @spec check_priority(notify_info(), notify_mail_conf()) -> {true, [string()]} | false.
%% @doc 通知情報、メール通知定義を元にメールを送信可否を確認する。
-spec check_priority(notify_info(), notify_mail_conf()) -> {true, [string()]} | false.
check_priority(_NotifyInfo=#notify_info{priority=Priority}, MailConf) ->
	case Priority of
		info ->
			case MailConf#notify_mail_conf.info_enabled of
				true -> {true, MailConf#notify_mail_conf.info_address};
				false -> false
			end;
		warning ->
			case MailConf#notify_mail_conf.warning_enabled of
				true -> {true, MailConf#notify_mail_conf.warning_address};
				false -> false
			end;
		error ->
			case MailConf#notify_mail_conf.error_enabled of
				true -> {true, MailConf#notify_mail_conf.error_address};
				false -> false
			end;
		unknown ->
			case MailConf#notify_mail_conf.unknown_enabled of
				true -> {true, MailConf#notify_mail_conf.unknown_address};
				false -> false
			end
	end.

%% @spec get_subject(notify_info(), mail_template_conf()) -> string().
%% @doc 通知情報、メールテンプレート定義を元にメールサブジェクトを取得する。
-spec get_subject(notify_info(), mail_template_conf()) -> string().
get_subject(NotifyInfo, _MailTemplateConf=#mail_template_conf{subject=Subject}) ->
	kesto_core_replace:replace(Subject, NotifyInfo).

%% @spec get_body(notify_info(), mail_template_conf()) -> string().
%% @doc 通知情報、メールテンプレート定義を元にメール本文を取得する。
-spec get_body(notify_info(), mail_template_conf()) -> string().
get_body(NotifyInfo, _MailTemplateConf=#mail_template_conf{body=Body}) ->
	kesto_core_replace:replace(Body, NotifyInfo).

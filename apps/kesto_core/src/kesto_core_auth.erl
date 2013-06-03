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
%% @doc 認証管理モジュール

-module(kesto_core_auth).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 put/1,
		 get/2,
		 update/1,
		 delete/2,
		 sign_in/4,
		 sign_out/1,
		 is_authenticated/3
		]).

%% @spec make_key(auth_conf()) -> string().
%% @doc ユーザ/ロール/システム定義からキーを生成する。
-spec make_key(auth_conf()) -> string().
make_key(Conf) when is_record(Conf, user) ->
	make_key(user, Conf#user.id);
make_key(Conf) when is_record(Conf, roll) ->
	make_key(roll, Conf#roll.id);
make_key(Conf) when is_record(Conf, system) ->
	make_key(system, Conf#system.id).

%% @spec make_key(string(), string()) -> string().
%% @doc 定義種別、定義IDからキーを生成する。
-spec make_key(auth_type(), string()) -> string().
make_key(Type, ID) ->
	lists:concat([atom_to_list(Type), "_", ID]).

%% @spec put(auth_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc ユーザ/ロール/システム定義をRiakに登録する。
-spec put(auth_conf()) -> ok | {error, atom()} | {error, term()}.
put(Conf) ->
	Key = make_key(Conf),
	try riak_object:new(?KESTO_CORE_AUTH_BUCKET, list_to_binary(Key), Conf) of
		Object ->
			lager:debug("ユーザ/ロール/システム定義用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("ユーザ/ロール/システム定義用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("ユーザ/ロール/システム定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("ユーザ/ロール/システム定義用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(auth_type(), string()) -> {ok, auth_conf()} | {error, atom()} | {error, term()}.
%% @doc Riakから認証定義種別とIDを使ってユーザ/ロール/システム定義を取得する。
-spec get(auth_type(), string()) -> {ok, auth_conf()} | {error, atom()} | {error, term()}.
get(Type, ID) ->
	Key = make_key(Type, ID),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_AUTH_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			lager:debug("ユーザ/ロール/システム定義用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("ユーザ/ロール/システム定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update(auth_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みのユーザ/ロール/システム定義を更新する。
-spec update(auth_conf()) -> ok | {error, atom()} | {error, term()}.
update(Conf) ->
	Key = make_key(Conf),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_AUTH_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Conf) of
				NewObj ->
					lager:debug("ユーザ/ロール/システム定義用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したユーザ/ロール/システム定義用riak_objectを登録しました。 : ~p", [NewObj]);
						{error, Error} ->
							lager:error("更新したユーザ/ロール/システム定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("ユーザ/ロール/システム定義用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("ユーザ/ロール/システム定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string(), string()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakから認証定義種別とIDを使ってユーザ/ロール/システム定義を削除する。
-spec delete(string(), string()) -> ok | {error, atom()} | {error, term()}.
delete(Type, ID) ->
	Key = make_key(Type, ID),
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_AUTH_BUCKET, list_to_binary(Key)) of
		ok ->
			lager:debug("ユーザ/ロール/システム定義用riak_objectを削除しました。 : ~p", [Key]);
		{error, Error} ->
			lager:error("ユーザ/ロール/システム定義用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec put_session(auth_session()) -> ok | {error, atom()} | {error, term()}.
%% @doc セッション定義をRiakに登録する。
-spec put_session(auth_session()) -> ok | {error, atom()} | {error, term()}.
put_session(Session) ->
	try riak_object:new(?KESTO_CORE_AUTH_SESSION_BUCKET, list_to_binary(Session#auth_session.id), Session) of
		Object ->
			lager:debug("セッション定義用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("セッション定義用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("セッション定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("セッション定義用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_session(user_id()) -> {ok, auth_session()} | {error, atom()} | {error, term()}.
%% @doc RiakからユーザIDを使ってセッション定義を取得する。
-spec get_session(user_id()) -> {ok, auth_session()} | {error, atom()} | {error, term()}.
get_session(UserID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_AUTH_SESSION_BUCKET, list_to_binary(UserID)) of
		{ok, Obj} ->
			lager:debug("セッション定義用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("セッション定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update_session(auth_session()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みのセッション定義を更新する。
-spec update_session(auth_session()) -> ok | {error, atom()} | {error, term()}.
update_session(Session) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_AUTH_SESSION_BUCKET, list_to_binary(Session#auth_session.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Session) of
				NewObj ->
					lager:debug("セッション定義用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したセッション定義用riak_objectを登録しました。 : ~p", [NewObj]);
						{error, Error} ->
							lager:error("更新したセッション定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("セッション定義用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("セッション定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete_session(user_id()) -> ok | {error, atom()} | {error, term()}.
%% @doc RiakからユーザIDを使ってセッション定義を削除する。
-spec delete_session(user_id()) -> ok | {error, atom()} | {error, term()}.
delete_session(UserID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_AUTH_SESSION_BUCKET, list_to_binary(UserID)) of
		ok ->
			lager:debug("セッション定義用riak_objectを削除しました。 : ~p", [UserID]);
		{error, Error} ->
			lager:error("セッション定義用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc セッションIDでセッション定義を取得する。
-spec find_session(string()) -> {ok, auth_session()} | {error, atom()} | {error, term()}.
find_session(SessionID) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj, _KeyData, _Arg) ->
				  Session = riak_object:get_value(Obj),
				  case Session#auth_session.session_id == SessionID of
					  true ->
						  [Session];
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_AUTH_SESSION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, []} ->
			{error, notfound};
		{ok, [Session | _Tail]} ->
			lager:debug("セッション定義を取得しました。 : ~p", [Session]),
			{ok, Session};
		{error, Error} ->
			lager:error("セッション定義の取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc セッションキーを生成する。
-spec make_session_id() -> string().
make_session_id() ->
	binary_to_list(base64url:encode(crypto:rand_bytes(256))).

%% @doc セッション有効期限を生成する。
-spec make_expiration_datetime() -> datetime().
make_expiration_datetime() ->
	Now = calendar:local_time(),
	Offset = kesto_core_config:expiration_date(),
	calendar:gregorian_seconds_to_datetime(
	  calendar:datetime_to_gregorian_seconds(Now) + Offset).

-include_lib("eunit/include/eunit.hrl").

%% @doc ユーザIDとパスワードによりサインインする。
-spec sign_in(user_id(), string(), string(), string()) -> 
		  {ok, auth_session()} | {error, atom()} | {error, term()}.
sign_in(UserID, Password, UserAgent, IpAddress) ->
	case get(user, UserID) of
		{ok, User} ->
			lager:debug("ユーザ定義用riak_objectを取得しました。 : ~p", [User]),
			case User#user.password == Password of
				true ->
					ExpirationDate = make_expiration_datetime(),
					Session = #auth_session{id=UserID,
											session_id=make_session_id(),
											expiration_datetime=ExpirationDate,
											user_agent=UserAgent, 
											ip_address=IpAddress},
					case put_session(Session) of
						ok ->
							{ok, Session};
						{error, Error} ->
							lager:error("サインインに失敗しました。 : ~p", [Error]),
							{error, Error}
					end;
				false ->
					lager:error("サインインに失敗しました。 : ~p", [unmatch]),
					{error, unmatch}
			end;
		{error, Error} ->
			lager:error("サインインに失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc セッションIDによりサインアウトする。
-spec sign_out(string()) -> ok | {error, atom()} | {error, term()}.
sign_out(SessionID) ->
	case find_session(SessionID) of
		{ok, Session} ->
			case delete_session(Session#auth_session.id) of
				ok ->
					ok;
				{error, Error} ->
					{error, Error}
			end;
		{error, Error} ->
			{error, Error}
	end.

%% @doc セッションIDにより認証済みかチェックする。
-spec is_authenticated(string(), string(), string()) -> 
		  {ok, auth_session()} | {error, atom()} | {error, term()}.
is_authenticated(SessionID, UserAgent, IpAddress) ->
	case find_session(SessionID) of
		{ok, Session} ->
			case Session#auth_session.expiration_datetime < calendar:local_time() of
				true ->
					{error, past_expiration_date};
				false ->
					case (Session#auth_session.user_agent == UserAgent) 
												  and (Session#auth_session.ip_address == IpAddress) of
						true ->
							ExpirationDate = make_expiration_datetime(),
							NewSession = 
								Session#auth_session{expiration_datetime=ExpirationDate},
							case update_session(NewSession) of
								ok ->
									{ok, NewSession};
								{error, Error} ->
									{error, Error} 
							end;
						false ->
							{error, unmatch}
					end
			end;
		{error, Error} ->
			{error, Error}
	end.
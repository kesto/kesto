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
%% @doc 認証管理用vnode

-module(kesto_core_auth_vnode).
-behaviour(riak_core_vnode).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
		 init/1,
		 terminate/2,
		 handle_command/3,
		 is_empty/1,
		 delete/1,
		 handle_handoff_command/3,
		 handoff_starting/2,
		 handoff_cancelled/1,
		 handoff_finished/2,
		 handle_handoff_data/2,
		 encode_handoff_item/2,
		 handle_coverage/4,
		 handle_exit/3]).

-export([
		 put/2,
		 get/3,
		 update/2,
		 delete/3,
		 sign_in/5,
		 sign_out/2,
		 is_authenticated/4
		]).

-record(state, {partition}).

-define(MASTER, kesto_core_auth_vnode_master).

start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, #state { partition=Partition }}.

%% @spec put(term(), auth_conf()) -> ok | {error, atom()}.
%% @doc ユーザ/ロール/システム定義をRiakに登録する。
-spec put(term(), auth_conf()) -> ok | {error, atom()}.
put(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{put, Conf},
										?MASTER).

%% @spec get(term(), auth_type(), string()) -> auth_conf() | {error, atom()}.
%% @doc Riakから認証定義種別とIDを使ってユーザ/ロール/システム定義を取得する。
-spec get(term(), auth_type(), string()) -> auth_conf() | {error, atom()}.
get(IdxNode, Type, ID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{get, Type, ID},
										?MASTER).

%% @spec update(term(), auth_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みのユーザ/ロール/システム定義を更新する。
-spec update(term(), auth_conf()) -> ok | {error, atom()} | {error, term()}.
update(IdxNode, Conf) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{update, Conf},
										?MASTER).

%% @spec delete(term(), string(), string()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakから認証定義種別とIDを使ってユーザ/ロール/システム定義を削除する。
-spec delete(term(), string(), string()) -> ok | {error, atom()} | {error, term()}.
delete(IdxNode, Type, ID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{delete, Type, ID},
										?MASTER).

%% @doc ユーザIDとパスワードによりサインインする。
-spec sign_in(term(), user_id(), string(), string(), string()) -> 
		  {ok, auth_session()} | {error, atom()} | {error, term()}.
sign_in(IdxNode, UseID, Password, UserAgent, IpAddress) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{sign_in, UseID, Password, UserAgent, IpAddress},
										?MASTER).

%% @doc セッションIDによりサインアウトする。
-spec sign_out(term(), string()) -> ok | {error, atom()} | {error, term()}.
sign_out(IdxNode, SessionID) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{sign_out, SessionID},
										?MASTER).

%% @doc セッションIDにより認証済みかチェックする。
-spec is_authenticated(term(), string(), string(), string()) -> 
		  {ok, auth_session()} | {error, atom()} | {error, term()}.
is_authenticated(IdxNode, SessionID, UserAgent, IpAddress) ->
	riak_core_vnode_master:sync_command(IdxNode,
										{is_authenticated, SessionID, UserAgent, IpAddress},
										?MASTER).

handle_command({put, Conf}, _Sender, State) ->
	lager:debug("put node : ~p", [Conf]),
	case kesto_core_auth:put(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({get, Type, ID}, _Sender, State) ->
	lager:debug("get node : ~s", [{Type, ID}]),
	case kesto_core_auth:get(Type, ID) of
		{ok, Obj} ->
			{reply, Obj, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({update, Conf}, _Sender, State) ->
	lager:debug("update node : ~p", [Conf]),
	case kesto_core_auth:update(Conf) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({delete, Type, ID}, _Sender, State) ->
	lager:debug("delete node : ~s", [{Type, ID}]),
	case kesto_core_auth:delete(Type, ID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({sign_in, UseID, Password, UserAgent, IpAddress}, _Sender, State) ->
	lager:debug("delete node : ~s", [{UseID, Password, UserAgent, IpAddress}]),
	case kesto_core_auth:sign_in(UseID, Password, UserAgent, IpAddress) of
		{ok, Session} ->
			{reply, Session, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({sign_out, SessionID}, _Sender, State) ->
	lager:debug("delete node : ~s", [{SessionID}]),
	case kesto_core_auth:sign_out(SessionID) of
		ok ->
			{reply, ok, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end;

handle_command({is_authenticated, SessionID, UserAgent, IpAddress}, _Sender, State) ->
	lager:debug("delete node : ~s", [{SessionID, UserAgent, IpAddress}]),
	case kesto_core_auth:is_authenticated(SessionID, UserAgent, IpAddress) of
		{ok, Session} ->
			{reply, Session, State};
		{error, Error} ->
			{reply, {error, Error}, State}
	end.

handle_handoff_command(_Message, _Sender, State) ->
	{noreply, State}.

handoff_starting(_TargetNode, State) ->
	{true, State}.

handoff_cancelled(State) ->
	{ok, State}.

handoff_finished(_TargetNode, State) ->
	{ok, State}.

handle_handoff_data(_Data, State) ->
	{reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
	<<>>.

is_empty(State) ->
	{true, State}.

delete(State) ->
	{ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
	{stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

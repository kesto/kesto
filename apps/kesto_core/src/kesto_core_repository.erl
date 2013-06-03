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
%% @doc リポジトリ管理モジュール

-module(kesto_core_repository).

-compile([{parse_transform, lager_transform}]).

-include("kesto_core.hrl").

-export([
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 assign/2,
		 unassign/2,
		 get_id_list/2,
		 get_list/2,
		 is_node/2,
		 is_scope/2,
		 get_facility_id/2,
		 get_node_facility_id_list/2
		]).

-export([
		 put/2
		]).

%% @spec put(#node{} | #scope{} -> ok | {error, atom()} | {error, Error}
%% @doc ノード/スコープをRiakに登録する。
put(Facility) when is_record(Facility, node) ->
	kesto_core_repository:put(Facility, Facility#node.id);
put(Facility) when is_record(Facility, scope) ->
	kesto_core_repository:put(Facility, Facility#scope.id);
put(_) ->
	{error, undefined}.

put(Facility, FacilityID) ->
	try riak_object:new(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID), Facility) of
		Object ->
			lager:debug("ノード/スコープ用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("ノード/スコープ用riak_objectを登録しました。 : ~p", [Object]),
					ok;
				{error, Error} ->
					lager:error("ノード/スコープ用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("ノード/スコープ用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(string()) -> #node{} | #scope{} | {error, atom()} | {error, Error}
%% @doc RiakからファシリティIDを使ってノード/スコープを取得する。
get(FacilityID) when is_list(FacilityID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID)) of
		{ok, Obj} ->
			lager:debug("ノード/スコープ用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("ノード/スコープ用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get(_) ->
	{error, undefined}.

%% @spec update(#node{} | #scope{}) -> ok | {error, atom()} | {error, Error}
%% @doc Riakに登録済みのノード/スコープを更新する。
update(Facility) when is_record(Facility, node) ->
	update(Facility, Facility#node.id);
update(Facility) when is_record(Facility, scope) ->
	update(Facility, Facility#scope.id);
update(_) ->
	{error, undefined}.

update(Facility, FacilityID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Facility) of
				NewObj ->
					lager:debug("ノード/スコープ用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したノード/スコープ用riak_objectを登録しました。 : ~p", [NewObj]),
							ok;
						{error, Error} ->
							lager:error("更新したノード/スコープ用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("ノード/スコープ用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("ノード/スコープ用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string()) -> ok | {error, atom()} | {error, Error}
%% @doc RiakからイベントIDを使ってノード/スコープを削除する。
delete(FacilityID) when is_list(FacilityID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID)) of
		ok ->
			lager:debug("ノード/スコープ用riak_objectを削除しました。 : ~p", [FacilityID]),
			ok;
		{error, Error} ->
			lager:error("ノード/スコープ用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
delete(_) ->
	{error, undefined}.

%% @spec assign(string(), [string()]) -> ok | {error, atom()} | {error, Error}
%% @doc スコープにノードを割り当て、Riakに登録/更新する。
assign(FacilityID, FacilityIDList) when is_list(FacilityID) and is_list(FacilityIDList) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID)) of
		{ok, Obj} ->
			Scope = riak_object:get_value(Obj),
			case set_children(Scope, FacilityIDList) of
				{ok, NewScope} ->
					case update(NewScope) of
						ok ->
							lager:debug("ノードを割り当てたスコープ用riak_objectを登録しました。 : ~p", [NewScope]),
							ok;
						{error, Error} ->
							lager:error("ノードを割り当てたスコープ用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end;
				{error, Error} ->
					lager:error("スコープ用riak_objectへのノード割り当てに失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("スコープ用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
assign(_, _) ->
	{error, undefined}.

set_children(Scope, []) -> {ok, Scope};
set_children(Scope, [FacilityID | Tail]) ->
	case is_node(FacilityID, all) of
		true ->
			Children = Scope#scope.children ++ [FacilityID],
			NewScope = Scope#scope{children=Children},
			set_children(NewScope, Tail);
		false ->
			lager:warning("登録されていないノードが指定されています。: ~p", [FacilityID]),
			{error, notfound}
	end.	

%% @spec unassign(string(), [string()]) -> ok | {error, atom()} | {error, Error}
%% @doc スコープからノードの割り当てを解除し、Riakに登録/更新する。
unassign(FacilityID, FacilityIDList) when is_list(FacilityID) and is_list(FacilityIDList) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_REPOSITORY_BUCKET, list_to_binary(FacilityID)) of
		{ok, Obj} ->
			Scope = riak_object:get_value(Obj),
			case unset_children(Scope, FacilityIDList) of
				{ok, NewScope} ->
					case update(NewScope) of
						ok ->
							lager:debug("ノードの割り当てを解除したスコープ用riak_objectを登録しました。 : ~p", [NewScope]),
							ok;
						{error, Error} ->
							lager:error("ノードの割り当てを解除したスコープ用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end;
				{error, Error} ->
					lager:error("スコープ用riak_objectへのノード割り当て解除に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("スコープ用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
unassign(_, _) ->
	{error, undefined}.

unset_children(Scope, []) -> {ok, Scope};
unset_children(Scope, [FacilityID | Tail]) ->
	case is_node(FacilityID, all) of
		true ->
			Children = Scope#scope.children -- [FacilityID],
			NewScope = Scope#scope{children=Children},
			unset_children(NewScope, Tail);
		false ->
			lager:warning("登録されていないノードが指定されています。: ~p", [FacilityID]),
			{error, notfound}
	end.

%% @spec get_id_list(atom(), atom()) -> {ok, [string()]} | {error, atom()} | {error, Error}
%% @doc ファシリティIDリストを取得する。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
get_id_list(node, Enabled) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, {E}) ->
				  Facility = riak_object:get_value(Obj),
				  case is_record(Facility, node) of
					  true ->
						  if
							  E == Facility#node.enabled ->
								  [Facility#node.id];
							  E == all ->
								  [Facility#node.id];
							  true ->
								  []
						  end;
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_REPOSITORY_BUCKET, [{map, {qfun, Map}, {Enabled}, true}]) of
		{ok, List} ->
			lager:debug("ファシリティIDリストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("ファシリティIDリストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get_id_list(scope, Enabled) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, Arg) ->
				  {Value} = Arg,
				  Facility = riak_object:get_value(Obj),
				  case is_record(Facility, scope) of
					  true ->
						  if
							  Value == Facility#scope.enabled ->
								  [Facility#scope.id];
							  Value == all ->
								  [Facility#scope.id];
							  true ->
								  []
						  end;
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_REPOSITORY_BUCKET, [{map, {qfun, Map}, {Enabled}, true}]) of
		{ok, List} ->
			lager:debug("ファシリティIDリストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("ファシリティIDリストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_list(atom(), atom()) -> {ok, [#node{} | #scope{}]} | {error, atom()} | {error, Error}
%% @doc ファシリティリストを取得する。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
get_list(node, Enabled) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, {E}) ->
				  Facility = riak_object:get_value(Obj),
				  case is_record(Facility, node) of
					  true ->
						  if
							  E == Facility#node.enabled ->
								  [Facility];
							  E == all ->
								  [Facility];
							  true ->
								  []
						  end;
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_REPOSITORY_BUCKET, [{map, {qfun, Map}, {Enabled}, true}]) of
		{ok, List} ->
			lager:debug("ファシリティリストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("ファシリティリストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end;
get_list(scope, Enabled) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, Arg) ->
				  {Value} = Arg,
				  Facility = riak_object:get_value(Obj),
				  case is_record(Facility, scope) of
					  true ->
						  if
							  Value == Facility#scope.enabled ->
								  [Facility];
							  Value == all ->
								  [Facility];
							  true ->
								  []
						  end;
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_REPOSITORY_BUCKET, [{map, {qfun, Map}, {Enabled}, true}]) of
		{ok, List} ->
			lager:debug("ファシリティリストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("ファシリティリストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec is_node(string(), atom()) -> true | false
%% @doc ファシリティIDがノードかチェックする。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
is_node(FacilityID, Enabled) ->
	{ok, List} = get_id_list(node, Enabled),
	lists:member(FacilityID, List).

%% @spec is_scope(string(), atom()) -> true | false
%% @doc ファシリティIDがスコープかチェックする。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
is_scope(FacilityID, Enabled) ->
	{ok, List} = get_id_list(scope, Enabled),
	lists:member(FacilityID, List).

%% @spec get_facility_id(string(), atom()) -> {ok, [string()]} | {error, atom()} | {error, Error}
%% @doc ファシリティIDリストを取得する。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
get_facility_id(Hostname, Enabled) ->
	{ok, List} = get_list(node, Enabled),
	get_facility_id(Hostname, List, []).
get_facility_id(Hostname, [], NodeList) when is_list(Hostname) ->
	{ok, NodeList};
get_facility_id(Hostname, [Node | T], NodeList) when is_list(Hostname) ->
	case lists:member(Hostname, Node#node.hostname) of
		true ->
			get_facility_id(Hostname, T, NodeList ++ [Node#node.id]);
		false ->
			case lists:member(Hostname, Node#node.ipv4) of
				true ->
					get_facility_id(Hostname, T, NodeList ++ [Node#node.id]);
				false ->
					case lists:member(Hostname, Node#node.ipv6) of
						true ->
							get_facility_id(Hostname, T, NodeList ++ [Node#node.id]);
						false ->
							get_facility_id(Hostname, T, NodeList)
					end
			end
	end.

%% @spec get_facility_id(string(), atom()) -> {ok, [string()]} | {error, atom()} | {error, Error}
%% @doc ファシリティIDリストを取得する。
%%      Enabledがallならば全て、trueならば有効なもののみ、falseならば無効なもののみ
get_node_facility_id_list(FacilityID, Enabled) when is_list(FacilityID) ->
	case kesto_core_repository:get(FacilityID) of
		{ok, Facility} ->
			case is_record(Facility, scope) of
				true ->
					case is_scope(Facility#scope.id, Enabled) of
						true ->
							get_node_facility_id_list(Facility#scope.children, Enabled, []);
						false ->
							{ok, []}
					end;
				false ->
					case is_node(Facility#node.id, Enabled) of
						true ->
							{ok, [Facility#node.id]};
						false ->
							{ok, []}
					end
			end;
		{error, _Error} ->
			{ok, []}
	end.
get_node_facility_id_list([], _, NodeList) ->
	{ok, NodeList};
get_node_facility_id_list([FacilityID | T], Enabled, NodeList) ->
	case is_node(FacilityID, Enabled) of
		true ->
			get_node_facility_id_list(T, Enabled, NodeList ++ [FacilityID]);
		false ->
			get_node_facility_id_list(T, Enabled, NodeList)
	end.

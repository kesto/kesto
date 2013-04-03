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
%% @doc セッションオペレーション情報管理用モジュール

-module(kesto_job_session_operation).

-compile([{parse_transform, lager_transform}]).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-export([
		 put/1,
		 get/1,
		 update/1,
		 delete/1,
		 get_list/1,
		 get_list/2,
		 get_list/4,
		 get_list/5,
		 make_operation_id/0,
		 setState/4
		]).

%% @doc セッションオペレーション情報をRiakに登録する。
-spec put(job_session_operation()) -> ok | {error, atom()} | {error, term()}.
put(Operation) ->
	try riak_object:new(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, list_to_binary(Operation#job_session_operation.id), Operation) of
		Object ->
			lager:debug("セッションオペレーション情報用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("セッションオペレーション情報用riak_objectを登録しました。 : ~p", [Object]);
				{error, Error} ->
					lager:error("セッションオペレーション情報用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("セッションオペレーション情報用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc RiakからジョブIDを使ってセッションオペレーション情報を取得する。
-spec get(operation_id()) -> {ok, job_session_operation()} | {error, atom()} | {error, term()}.
get(OperationID) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, list_to_binary(OperationID)) of
		{ok, Obj} ->
			lager:debug("セッションオペレーション情報用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("セッションオペレーション情報用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakに登録済みのセッションオペレーション情報を更新する。
-spec update(job_session_operation()) -> ok | {error, atom()} | {error, term()}.
update(Operation) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, list_to_binary(Operation#job_session_operation.id)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Operation) of
				NewObj ->
					lager:debug("セッションオペレーション情報用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したセッションオペレーション情報用riak_objectを登録しました。 : ~p", [NewObj]);
						{error, Error} ->
							lager:error("更新したセッションオペレーション情報用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("セッションオペレーション情報用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("セッションオペレーション情報用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakからセッションオペレーション情報IDを使ってセッションオペレーション情報を削除する。
-spec delete(operation_id()) -> ok | {error, atom()} | {error, term()}.
delete(OperationID) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, list_to_binary(OperationID)) of
		ok ->
			lager:debug("セッションオペレーション情報用riak_objectを削除しました。 : ~p", [OperationID]);
		{error, Error} ->
			lager:error("セッションオペレーション情報用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakからセッションオペレーション情報リストを取得する。
-spec get_list(job_operation_state() | all) -> {ok, [job_session_operation()]} | {error, atom()} | {error, term()}.
get_list(State) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Operation = riak_object:get_value(Obj),
				  case (State == all) or (State == Operation#job_session_operation.state) of
					  true -> [Operation];
					  false -> []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("セッションオペレーション情報リストを取得しました。 : ~p", [List]),
			{ok, 
			 lists:sort(fun(X, Y) ->
								X#job_session_operation.create_timestamp =< Y#job_session_operation.create_timestamp
						end,
						List)};
		{error, Error} ->
			lager:error("セッションオペレーション情報リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.
-spec get_list(session_id(), job_operation_state() | all) -> 
		  {ok, [job_session_operation()]} | {error, atom()} | {error, term()}.
get_list(SessionID, State) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Operation = riak_object:get_value(Obj),
				  case (Operation#job_session_operation.session_id == SessionID)
					  and ((State == all) or (State == Operation#job_session_operation.state)) of
					  true -> [Operation];
					  false -> []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("セッションオペレーション情報リストを取得しました。 : ~p", [List]),
			{ok, 
			 lists:sort(fun(X, Y) ->
								X#job_session_operation.create_timestamp =< Y#job_session_operation.create_timestamp
						end,
						List)};
		{error, Error} ->
			lager:error("セッションオペレーション情報リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.
-spec get_list(session_id(), group_id(), job_id(), job_operation_state() | all) -> 
		  {ok, [job_session_operation()]} | {error, atom()} | {error, term()}.
get_list(SessionID, GroupID, JobID, State) when is_atom(State) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Operation = riak_object:get_value(Obj),
				  case (Operation#job_session_operation.session_id == SessionID)
					  and (Operation#job_session_operation.group_id == GroupID) 
					  and (Operation#job_session_operation.job_id == JobID) 
					  and ((State == all) or (State == Operation#job_session_operation.state)) of
					  true -> [Operation];
					  false -> []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("セッションオペレーション情報リストを取得しました。 : ~p", [List]),
			{ok, 
			 lists:sort(fun(X, Y) ->
								X#job_session_operation.create_timestamp =< Y#job_session_operation.create_timestamp
						end,
						List)};
		{error, Error} ->
			lager:error("セッションオペレーション情報リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.
-spec get_list(session_id(), group_id(), job_id(), facility_id(), job_operation_state() | all) ->
		  {ok, [job_session_operation()]} | {error, atom()} | {error, term()}.
get_list(SessionID, GroupID, JobID, FacilityID, State) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Operation = riak_object:get_value(Obj),
				  case (Operation#job_session_operation.session_id == SessionID) 
					  and (Operation#job_session_operation.group_id == GroupID) 
					  and (Operation#job_session_operation.job_id == JobID) 
					  and (Operation#job_session_operation.facility_id == FacilityID) 
					  and ((State == all) or (State == Operation#job_session_operation.state)) of
					  true -> [Operation];
					  false -> []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_JOB_SESSION_OPERATION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("セッションオペレーション情報リストを取得しました。 : ~p", [List]),
			{ok, 
			 lists:sort(fun(X, Y) ->
								X#job_session_operation.create_timestamp =< Y#job_session_operation.create_timestamp
						end,
						List)};
		{error, Error} ->
			lager:error("セッションオペレーション情報リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc オペレーションIDを生成する。
-spec make_operation_id() -> operation_id().
make_operation_id() -> 
	make_operation_id(0).
-spec make_operation_id(integer()) -> operation_id().
make_operation_id(Count) -> 
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	ID = lists:concat([erlang:integer_to_list(Year), 
					   string:right(erlang:integer_to_list(Month), 2, $0), 
					   string:right(erlang:integer_to_list(Day), 2, $0), 
					   "_", 
					   string:right(erlang:integer_to_list(Hour), 2, $0), 
					   string:right(erlang:integer_to_list(Minute), 2, $0), 
					   string:right(erlang:integer_to_list(Second), 2, $0), 
					   "_", 
					   string:right(erlang:integer_to_list(Count), 3, $0)]),
	case kesto_job_session_operation:get(ID) of
		{ok, _Operation} ->
			make_operation_id(Count + 1);
		{error, notfound} ->
			ID
	end.

%% セッションオペレーション情報に状態を設定する。
-spec setState(operation_id() | undefined, 
			   job_operation_state(), 
			   string(), 
			   datetime() | undefined) ->
		  ok | {error, term()}.
setState(OperationID, State, Message, Timestamp) ->
	case OperationID of
		undefined -> ok;
		_ ->
			case kesto_job_session_operation:get(OperationID) of
				{ok, Info} ->
					NewInfo = case State of
								  running ->
									  Info#job_session_operation{state=State,
																 start_timestamp=Timestamp};
								  'end' ->
									  Info#job_session_operation{state=State,
																 message=Message,
																 end_timestamp=Timestamp};
								  error ->
									  Info#job_session_operation{state=State,
																 message=Message,
																 end_timestamp=Timestamp}
							  end,
					case update(NewInfo) of
						ok -> ok;
						{error, Error} -> {error, Error}
					end;
				{error, Error} -> {error, Error}
			end
	end.
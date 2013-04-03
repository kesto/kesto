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
%% @doc セッション情報管理用モジュール

-module(kesto_job_session_info).

-compile([{parse_transform, lager_transform}]).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-export([
		 put/1,
		 get/1,
		 get/3,
		 get/4,
		 update/1,
		 delete/1,
		 delete/3,
		 delete/4,
		 get_list/2,
		 get_list/3,
		 get_list/5,
		 get_list/6
		]).

%% @doc セッション情報をRiakに登録する。
-spec put(job_session_info()) -> ok | {error, atom()} | {error, term()}.
put(SessionInfo) ->
	Key = make_key(SessionInfo),
	try riak_object:new(?KESTO_CORE_JOB_SESSION_BUCKET, list_to_binary(Key), SessionInfo) of
		Object ->
			lager:debug("セッション情報用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("セッション情報用riak_objectを登録しました。 : ~p", [Object]);
				{error, Error} ->
					lager:error("セッション情報用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("セッション情報用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakからセッション情報を取得する。
-spec get(session_id(), group_id(), job_id()) -> {ok, job_session_job()} | {error, atom()} | {error, term()}.
get(SessionID, GroupID, JobID) ->
	kesto_job_session_info:get(make_key(SessionID, GroupID, JobID)).
-spec get(session_id(), group_id(), job_id(), string()) -> {ok, job_session_node()} | {error, atom()} | {error, term()}.
get(SessionID, GroupID, JobID, FacilityID) ->
	kesto_job_session_info:get(make_key(SessionID, GroupID, JobID, FacilityID)).
-spec get(string()) -> {ok, job_session_info()} | {error, atom()} | {error, term()}.
get(Key) ->
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_SESSION_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			lager:debug("セッション情報用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("セッション情報用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakに登録済みのセッション情報を更新する。
-spec update(job_session_info()) -> ok | {error, atom()} | {error, term()}.
update(SessionInfo) ->
	Key = make_key(SessionInfo),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_SESSION_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, SessionInfo) of
				NewObj ->
					lager:debug("セッション情報用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したセッション情報用riak_objectを登録しました。 : ~p", [NewObj]);
						{error, Error} ->
							lager:error("更新したセッション情報用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("セッション情報用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("セッション情報用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakからセッション情報を削除する。
-spec delete(session_id(), group_id(), job_id()) -> ok | {error, atom()} | {error, term()}.
delete(SessionID, GroupID, JobID) ->
	delete(make_key(SessionID, GroupID, JobID)).
-spec delete(session_id(), group_id(), job_id(), string()) -> ok | {error, atom()} | {error, term()}.
delete(SessionID, GroupID, JobID, FacilityID) ->
	delete(make_key(SessionID, GroupID, JobID, FacilityID)).
-spec delete(session_id()) -> ok | {error, atom()} | {error, term()}.
delete(Key) ->
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_JOB_SESSION_BUCKET, list_to_binary(Key)) of
		ok ->
			lager:debug("セッション情報用riak_objectを削除しました。 : ~p", [Key]);
		{error, Error} ->
			lager:error("セッション情報用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc Riakからセッション情報リストを取得する。
-spec get_list(job_state() | all, 
			   job_session | job_session_job | job_session_node | all) -> 
		  {ok, [job_session_info()]} | {error, atom()} | {error, term()}.
get_list(State, Record) ->
	get_list(all, all, all, all, State, Record).
-spec get_list(session_id(),
			   job_state() | all, 
			   job_session | job_session_job | job_session_node | all) -> 
		  {ok, [job_session_job()]} | {error, atom()} | {error, term()}.
get_list(SessionID, State, Record) ->
	get_list(SessionID, all, all, all, State, Record).
-spec get_list(session_id() | all, 
			   group_id() | all, 
			   job_id() | all, 
			   job_state() | all, 
			   job_session | job_session_job | job_session_node | all) -> 
		  {ok, [job_session_info()]} | {error, atom()} | {error, term()}.
get_list(SessionID,  GroupID, JobID, State, Record) ->
	get_list(SessionID, GroupID, JobID, all, State, Record).
-spec get_list(session_id() | all, 
			   group_id() | all, 
			   job_id() | all, 
			   facility_id() | all, 
			   job_state() | all, 
			   job_session | job_session_job | job_session_node | all) -> 
		  {ok, [job_session_info()]} | {error, atom()} | {error, term()}.
get_list(SessionID, GroupID, JobID, FacilityID, State, Record) ->
	{ok, Client} = riak:local_client(),
	Map = fun(Obj,  _KeyData, _Arg) ->
				  SessionInfo = riak_object:get_value(Obj),
				  case (Record == all) or (is_record(SessionInfo, Record)) of
					  true ->
						  case (State == all) or (State == get_state(SessionInfo)) of
							  true ->
								  case (SessionID == all) or (SessionID == get_session_id(SessionInfo)) of
									  true ->
										  case (GroupID == all) or (GroupID == get_group_id(SessionInfo)) of
											  true ->
												  case (JobID == all) or (JobID == get_job_id(SessionInfo)) of
													  true ->
														  case (FacilityID == all) or (FacilityID == get_facility_id(SessionInfo)) of
															  true -> [SessionInfo];
															  false -> []
														  end;
													  false -> []
												  end;
											  false -> []
										  end;
									  false -> []
								  end;
							  false -> []
						  end;
					  false -> []
				  end
		  end,
	case Client:mapred_bucket(?KESTO_CORE_JOB_SESSION_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("セッションジョブ情報リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("セッションジョブ情報リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @doc セッション情報からキーを生成する。
-spec make_key(job_session_job() | session_id()) -> string().
make_key(Session) when is_record(Session, job_session) ->
	Session#job_session.id;
make_key(SessionJob) when is_record(SessionJob, job_session_job) ->
	lists:concat([SessionJob#job_session_job.id, 
				  "_", 
				  SessionJob#job_session_job.group_id,
				  "_", 
				  SessionJob#job_session_job.job_id]);
make_key(SessionNode) when is_record(SessionNode, job_session_node) ->
	lists:concat([SessionNode#job_session_node.id, 
				  "_", 
				  SessionNode#job_session_node.group_id,
				  "_", 
				  SessionNode#job_session_node.job_id, 
				  "_", 
				  SessionNode#job_session_node.facility#node.id]);
%% @doc セッションIDからキーを生成する。
make_key(SessionID) when is_list(SessionID) ->
	SessionID.
%% @doc セッションID, グループID、ジョブIDからキーを生成する。
-spec make_key(session_id(), group_id(), job_id()) -> string().
make_key(SessionID, GroupID, JobID) ->
	lists:concat([SessionID, 
				  "_", 
				  GroupID,
				  "_", 
				  JobID]).
%% @doc セッションID, グループID、ジョブID、ファシリティIDからキーを生成する。
-spec make_key(session_id(), group_id(), job_id(), string()) -> string().
make_key(SessionID, GroupID, JobID, FacilityID) ->
	lists:concat([SessionID, 
				  "_", 
				  GroupID,
				  "_", 
				  JobID, 
				  "_", 
				  FacilityID]).

%% @doc セッション情報からジョブ状態を取得する。
-spec get_state(job_session_info()) -> job_state().
get_state(Session) when is_record(Session, job_session) ->
	Session#job_session.state;
get_state(SessionJob) when is_record(SessionJob, job_session_job) ->
	SessionJob#job_session_job.state;
get_state(SessionNode) when is_record(SessionNode, job_session_node) ->
	SessionNode#job_session_node.state.

%% @doc セッション情報からセッションIDを取得する。
-spec get_session_id(job_session_info()) -> session_id().
get_session_id(Session) when is_record(Session, job_session) ->
	Session#job_session.id;
get_session_id(SessionJob) when is_record(SessionJob, job_session_job) ->
	SessionJob#job_session_job.id;
get_session_id(SessionNode) when is_record(SessionNode, job_session_node) ->
	SessionNode#job_session_node.id.

%% @doc セッション情報からグループIDを取得する。
-spec get_group_id(job_session_info()) -> group_id().
get_group_id(Session) when is_record(Session, job_session) ->
	Session#job_session.group_id;
get_group_id(SessionJob) when is_record(SessionJob, job_session_job) ->
	SessionJob#job_session_job.group_id;
get_group_id(SessionNode) when is_record(SessionNode, job_session_node) ->
	SessionNode#job_session_node.group_id.

%% @doc セッション情報からジョブIDを取得する。
-spec get_job_id(job_session_info()) -> job_id().
get_job_id(Session) when is_record(Session, job_session) ->
	Session#job_session.job_id;
get_job_id(SessionJob) when is_record(SessionJob, job_session_job) ->
	SessionJob#job_session_job.job_id;
get_job_id(SessionNode) when is_record(SessionNode, job_session_node) ->
	SessionNode#job_session_node.job_id.

%% @doc セッション情報からファシリティIDを取得する。
-spec get_facility_id(job_session_info()) -> facility_id().
get_facility_id(Session) when is_record(Session, job_session) ->
	notfound;
get_facility_id(SessionJob) when is_record(SessionJob, job_session_job) ->
	notfound;
get_facility_id(SessionNode) when is_record(SessionNode, job_session_node) ->
	SessionNode#job_session_node.facility#node.id.

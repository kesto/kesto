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
%% @doc ジョブ定義管理用モジュール

-module(kesto_job_conf).

-compile([{parse_transform, lager_transform}]).

-include("kesto_job.hrl").
-include_lib("kesto_core/include/kesto_core.hrl").

-export([
		 put/1,
		 get/2,
		 update/1,
		 delete/2,
		 get_list/1,
		 get_relation_list/2
		]).

%% @spec put(job_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc ジョブ定義をRiakに登録する。
-spec put(job_conf()) -> ok | {error, atom()} | {error, term()}.
put(Conf) ->
	Key = make_key(Conf),
	try riak_object:new(?KESTO_CORE_JOB_CONF_BUCKET, list_to_binary(Key), Conf) of
		Object ->
			lager:debug("ジョブ定義用riak_objectを作成しました。 : ~p", [Object]),
			{ok, Client} = riak:local_client(),
			case Client:put(Object, kesto_core_config:put_option()) of
				ok ->
					lager:debug("ジョブ定義用riak_objectを登録しました。 : ~p", [Object]);
				{error, Error} ->
					lager:error("ジョブ定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
					{error, Error}
			end
	catch
		_:Error ->
			lager:error("ジョブ定義用riak_objectの作成に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get(string(), string()) -> {ok, job_conf()} | {error, atom()} | {error, term()}
%% @doc RiakからグループID、ジョブIDを使ってジョブ定義を取得する。
-spec get(string(), string()) -> {ok, job_conf()} | {error, atom()} | {error, term()}.
get(GroupID, JobID) ->
	Key = make_key(GroupID, JobID),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_CONF_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			lager:debug("ジョブ定義用riak_objectを取得しました。 : ~p", [Obj]),
			{ok, riak_object:get_value(Obj)};
		{error, Error} ->
			lager:error("ジョブ定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec update(job_conf()) -> ok | {error, atom()} | {error, term()}.
%% @doc Riakに登録済みのジョブ定義を更新する。
-spec update(job_conf()) -> ok | {error, atom()} | {error, term()}.
update(Conf) ->
	Key = make_key(Conf),
	{ok, Client} = riak:local_client(),
	case Client:get(?KESTO_CORE_JOB_CONF_BUCKET, list_to_binary(Key)) of
		{ok, Obj} ->
			try riak_object:update_value(Obj, Conf) of
				NewObj ->
					lager:debug("ジョブ定義用riak_objectを更新しました。 : ~p", [NewObj]),
					case Client:put(NewObj, kesto_core_config:put_option()) of
						ok ->
							lager:debug("更新したジョブ定義用riak_objectを登録しました。 : ~p", [NewObj]);
						{error, Error} ->
							lager:error("更新したジョブ定義用riak_objectの登録に失敗しました。 : ~p", [Error]),
							{error, Error}
					end
			catch
				_:Error ->
					lager:error("ジョブ定義用riak_objectの更新に失敗しました。 : ~p", [Error]),
					{error, Error}
			end;
		{error, Error} ->
			lager:error("ジョブ定義用riak_objectの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec delete(string(), string()) -> ok | {error, atom()} | {error, term()}.
%% @doc RiakからグループID、ジョブIDを使ってジョブ定義を削除する。
-spec delete(string(), string()) -> ok | {error, atom()} | {error, term()}.
delete(GroupID, JobID) ->
	Key = make_key(GroupID, JobID),
	{ok, Client} = riak:local_client(),
	case Client:delete(?KESTO_CORE_JOB_CONF_BUCKET, list_to_binary(Key)) of
		ok ->
			lager:debug("ジョブ定義用riak_objectを削除しました。 : ~p", [Key]);
		{error, Error} ->
			lager:error("ジョブ定義用riak_objectの削除に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec make_key(job_conf()) -> string().
%% @doc ジョブ定義からキーを生成する。
-spec make_key(job_conf()) -> string().
make_key(Conf) ->
	lists:concat([Conf#job_conf.group_id, 
				  "_", 
				  Conf#job_conf.id]).
%% @spec make_key(string(), string()) -> string().
%% @doc グループID、ジョブIDからキーを生成する。
-spec make_key(string(), string()) -> string().
make_key(GroupID, JobID) ->
	lists:concat([GroupID,
				  "_", 
				  JobID]).

%% @spec get_list(job_type()) -> {ok, [job_conf()]} | {error, atom()} | {error, term()}.
%% @doc Riakからジョブ定義リストを取得する。
-spec get_list(job_type()) -> {ok, [job_conf()]} | {error, atom()} | {error, term()}.
get_list(Type) ->
	Map = fun(Obj,  _KeyData, _Arg) ->
				  Conf = riak_object:get_value(Obj),
				  case Conf#job_conf.type == Type of
					  true ->
						  [Conf];
					  false ->
						  []
				  end
		  end,
	case riak_kv_mrc_pipe:mapred(?KESTO_CORE_JOB_CONF_BUCKET, [{map, {qfun, Map}, none, true}]) of
		{ok, List} ->
			lager:debug("ジョブ定義リストを取得しました。 : ~p", [List]),
			{ok, List};
		{error, Error} ->
			lager:error("ジョブ定義リストの取得に失敗しました。 : ~p", [Error]),
			{error, Error}
	end.

%% @spec get_relation_list(string()) -> [{string(), job_conf()}].
%% @doc Riakから指定されたジョブIDに関連するジョブ定義リストを取得する。
-spec get_relation_list(string(), string()) -> [{string(), job_conf()}].
get_relation_list(GroupID, JobID) ->
	List = get_relation_list_([{GroupID, JobID}], dict:new()),
	dict:to_list(List).

-spec get_relation_list_([{string(), string()}], dict()) -> dict().
get_relation_list_([], Dict) ->
	Dict;
get_relation_list_([{GroupID, JobID} | T], Dict) ->
	case dict:is_key({GroupID, JobID}, Dict) of
		true ->
			get_relation_list_(T, Dict);
		false ->
			case kesto_job_conf:get(GroupID, JobID) of
				{ok, Conf} ->
					Dict2 = dict:store({GroupID, JobID}, Conf, Dict),
					Dict3 = get_relation_list_(Conf#job_conf.child_id, Dict2),
					Dict4 = get_relation_list_(Conf#job_conf.next_id, Dict3),
					get_relation_list_(T, Dict4);
				{error, notfound} ->
					get_relation_list_(T, Dict)
			end
	end.

%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_model_users).
-extends(maru_model_base).

-include_lib("maru_models/include/maru_model.hrl").
-include_lib("maru_models/include/jsonerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([find/1,
         to_json/1,
         to_record/1]).

-record(maru_model_users, {id = ossp_uuid:make(v1, text) :: maru_model_types:maru_key(),
                           fullname                      :: maru_model_types:maru_string(),
                           username                      :: maru_model_types:maru_string(),
                           email                         :: maru_model_types:maru_email(),
                           password                      :: maru_model_types:maru_password()}).

%%%===================================================================
%%% API
%%%===================================================================

find(Criteria) when is_list(Criteria) ->
    maru_db:find(?MODULE, Criteria);
find(Criteria) when is_tuple(Criteria)->
    maru_db:find(?MODULE, [Criteria]).

to_json(Record) ->
    ?record_to_json(?MODULE, Record).

to_record(JSON) ->
    ?json_to_record(?MODULE, JSON).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Test functions
%%%===================================================================

user_creation_test() ->
    User = new([{id, <<"TEST1">>},
                 {fullname, <<"User1">>},
                 {username, <<"UserName1">>},
                 {email, <<"UserEmail@example.com">>},
                 {password, <<"UserPassword1">>}]),

    ?assertEqual(get(username, User), <<"UserName1">>).


user_json_test() ->
    JSON = <<"{\"maru_model_users\":{\"id\":null,\"fullname\":null,\"username\":\"UserName2\",\"email\":null,\"password\":null}}">>,
    User = to_record(JSON),
    ?assertEqual(get(username, User), <<"UserName2">>),

    User2 = list_to_binary(lists:flatten(io_lib:format("~s", [to_json(User)]))),
    ?assertEqual(User2, JSON).

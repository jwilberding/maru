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

-include("maru_model.hrl").
-include("jsonerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([find/1,
         to_json/1,
         to_record/1]).

-record(maru_model_users, {id = ossp_uuid:make(v1, text) :: maru_model_types:maru_key(),
                           fullname                      :: maru_model_types:maru_string(),
                           username                      :: maru_model_types:maru_string(),
                           email                         :: maru_model_types:maru_email(),
                           password                      :: maru_model_types:maru_password(),
                           accounts=[]                   :: [maru_model_types:maru_key()]}).

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
                 {firstname, <<"UserFirst1">>},
                 {lastname, <<"UserLast1">>},
                 {username, <<"UserName1">>},
                 {email, <<"UserEmail@example.com">>},
                 {password, <<"UserPassword1">>},
                 {accounts, []}]),

    ?assertEqual(get(username, User), <<"UserName1">>).


user_json_test() ->
    JSON = "{\"id\":null,\"firstname\":null,\"lastname\":null,\"username\":\"UserName2\",\"email\":null,\"password\":null,\"accounts\":null}",
    User = to_record(JSON),
    ?assertEqual(get(username, User), <<"UserName2">>),

    User2 = lists:flatten(io_lib:format("~s", [to_json(User)])),
    ?assertEqual(User2, JSON).


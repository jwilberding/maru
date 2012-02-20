%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_resource_authenticate).

%% API
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         process_post/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("maru_web/include/maru_web.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init(term()) -> {ok, record(ctx)}.
init(_) ->
    {ok, #ctx{}}.

-spec allowed_methods(term(), term()) -> tuple().
allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

-spec resource_exists(term(), term()) -> {true, term(), term()}.
resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

-spec process_post(term(), term()) -> {term(), term(), term()}.
process_post(ReqData, Ctx) ->
    Username = wrq:get_qs_value("username", ReqData),
    Password = wrq:get_qs_value("password", ReqData),
    RememberMe = maru_web_utils:checkbox_value_to_bool(wrq:get_qs_value("remember_me", ReqData)),

    case maru_web_authenticate:is_valid(Username, Password) of
        {true, UserId} ->
            Cookie = maru_web_sessions:set_new_client_session_id(RememberMe, UserId),
            NewReqData = wrq:set_resp_header("Set-Cookie", Cookie, ReqData),
            {true, NewReqData, Ctx};
        false ->
            {false, ReqData, Ctx}
    end.

to_json(ReqData, Ctx) ->
    case maru_web_sessions:is_valid(ReqData) of
        true ->
            UserId = maru_web_sessions:get_user_id(ReqData),
            [UserRecord] = maru_model_users:find([{id, UserId}]),
            User = maru_model_users:to_json(UserRecord),
            {User, ReqData, Ctx};
        false ->
            {{halt, 404}, wrq:set_resp_header("Location", "/login.html", ReqData), Ctx}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

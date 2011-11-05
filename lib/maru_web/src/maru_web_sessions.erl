%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_web_sessions).

%% API
-export([set_new_client_session_id/2,
	 get_user_id/1,
         client_session_id/1,
	 is_valid/1]).

-export_type([session_id/0,
              rd/0]).

-type session_id() :: list().
-type rd() :: record().

%%%===================================================================
%%% API
%%%===================================================================

-spec set_new_client_session_id(string(), maru_model_types:maru_key()) -> string().
set_new_client_session_id(RememberMe, UserId) ->
    Session = maru_model_sessions:new([{user_id, UserId}]),
    maru_model_sessions:save(Session),
    maru_model_sessions:get_session_cookie(RememberMe, Session).

-spec client_session_id(rd()) -> undefined | session_id().
client_session_id(ReqData) ->
    case wrq:get_cookie_value("SESSIONID", ReqData) of
        undefined ->
            undefined;
        SessionIDString ->
            list_to_binary(SessionIDString)
    end.

get_user_id(ReqData) ->
    SessionId = client_session_id(ReqData),
    [Session] = maru_model_sessions:find([{id, SessionId}]),
    maru_model_sessions:get(user_id, Session).

is_valid(ReqData) ->
    SessionId = client_session_id(ReqData),
    maru_model_sessions:is_valid(SessionId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

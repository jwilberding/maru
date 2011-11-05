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
-export([set_new_client_session_id/1,
         client_session_id/1]).

-export_type([session_id/0,
              rd/0]).

-type session_id() :: list().
-type rd() :: record().

%%%===================================================================
%%% API
%%%===================================================================

-spec set_new_client_session_id(string()) -> string().
set_new_client_session_id(RememberMe) ->
    Session = maru_model_sessions:new(),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

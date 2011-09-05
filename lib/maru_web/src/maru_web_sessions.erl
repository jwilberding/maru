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
         set_client_session_id/2,
         client_session_id/1]).

-export_type([session_id/0,
              rd/0]).

-type session_id() :: list().
-type rd() :: record().

%%%===================================================================
%%% API
%%%===================================================================

-spec set_new_client_session_id(rd()) -> rd().
set_new_client_session_id(ReqData) ->
    Session = new(),
    maru_model_sessions:save(Session),
    set_client_session_id(ReqData, Session).

-spec set_client_session_id(rd(), record()) -> rd().
set_client_session_id(ReqData, Session) ->
    wrq:set_resp_header("Set-Cookie", "SESSIONID="++ binary_to_list(maru_model_sessions:get(id, Session)) ++"; Path=/", ReqData).

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

-spec new() -> session_id().
new() ->
    maru_model_sessions:new().

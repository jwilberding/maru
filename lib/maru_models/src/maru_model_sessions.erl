 %%%-------------------------------------------------------------------
 %%% @author Tristan Sloughter <>
 %%% @copyright (C) 2011, Tristan Sloughter
 %%% @doc
 %%%
 %%% @end
 %%% Created :  5 Jul 2011 by Tristan Sloughter <>
 %%%-------------------------------------------------------------------
-module(maru_model_sessions).
-extends(maru_model_base).

-include_lib("maru_models/include/maru_model.hrl").
-include_lib("maru_models/include/jsonerl.hrl").

 %% API
 -export([find/1,
	  get_session_cookie/2,
	  delete/1,
	  is_valid/1]).

 -record(maru_model_sessions, {id = ossp_uuid:make(v1, text)    :: maru_model_types:maru_key(),
                               user_id                          :: maru_model_types:maru_key(),
                               created=maru_idioms:time_in_seconds() :: integer()}).

-define(EXPIRE, 1209600). %% 2 weeks

%%%===================================================================
%%% API
%%%===================================================================

find(Criteria) when is_list(Criteria) ->
    maru_db:find(?MODULE, Criteria);
find(Criteria) when is_tuple(Criteria)->
    maru_db:find(?MODULE, [Criteria]).

get_session_cookie(true, Session) ->
    get_session_cookie_(Session, [{max_age, ?EXPIRE}, {path, "/"}]);
get_session_cookie(false, Session) ->
    get_session_cookie_(Session, [{path, "/"}]).

delete(Key) ->
    maru_db:delete(?MODULE, Key).

-spec is_valid(list()) -> true | false.
is_valid(SessionID) ->
    case find({id, SessionID}) of
        not_found ->
            false;
        [Session] ->
            not(expired(Session))
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

expired(Session) ->
    (maru_idioms:time_in_seconds() - Session#maru_model_sessions.created) > ?EXPIRE.


get_session_cookie_(Session, Options) ->
    {_, Value} = mochiweb_cookies:cookie("SESSIONID", binary_to_list(maru_model_sessions:get(id, Session)), Options),
    Value.

%%%===================================================================
%%% Test functions
%%%===================================================================

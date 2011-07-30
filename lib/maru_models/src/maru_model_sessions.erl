 %%%-------------------------------------------------------------------
 %%% @author Tristan Sloughter <>
 %%% @copyright (C) 2011, Tristan Sloughter
 %%% @doc
 %%%
 %%% @end
 %%% Created :  5 Jul 2011 by Tristan Sloughter <>
 %%%-------------------------------------------------------------------
 -module(maru_model_sessions).

 -include_lib("maru_models/include/maru_model.hrl").

 %% API
 -export([is_valid/1]).

 -record(maru_model_sessions, {id = ossp_uuid:make(v1, text)    :: maru_model_types:maru_key(),
                               user_id                          :: maru_model_types:maru_key(),
                               created=idioms:time_in_seconds() :: integer()}).

%% never expire session
-define(INFINTE_SESSION, true).
-define(EXPIRE, infinity).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_valid(list()) -> true | false.
is_valid(SessionID) ->
    case maru_db:get(?MODULE, SessionID) of
        not_found ->
            false;
        Session ->
            not(expired(Session))
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(INFINITE_SESSION).
expired(_Session) ->
    false.
-else.
expired(Session) ->
    (idioms:time_in_seconds() - Session#maru_model_sessions.created) > ?EXPIRE.
-endif.

%%%===================================================================
%%% Test functions
%%%===================================================================



%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_web_authenticate).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([is_valid/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_valid(binary() | list(), list()) -> true | false.
is_valid(Username, Password) when is_list(Username),
                                  is_list(Password) ->
    is_valid(maru_model_types:convert(maru_string, Username), Password);
is_valid(Username, Password) when is_binary(Username),
                                  is_list(Password) ->
    case maru_model_users:find([{username, Username}]) of
        not_found ->
            false;
        [User] ->
            UserPassword = maru_model_users:get(password, User),
	    UserId = maru_model_users:get(id, User),
            {{ok, UserPassword} =:= bcrypt:hashpw(Password, UserPassword), UserId}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Test functions
%%%===================================================================

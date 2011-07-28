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

-spec is_valid(binary(), list()) -> true | false.
is_valid(Username, Password) when is_binary(Username),
                                  is_list(Password) ->
    case maru_model_users:find([{username, Username}]) of
        not_found ->
            false;
        User ->
            UserPassword = maru_model_users:get(password, User),
            UserPassword =:= bcrypt:hashpw(Password, UserPassword)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Test functions
%%%===================================================================


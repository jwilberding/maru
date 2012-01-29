%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_idioms).

%% API
-export([time_in_seconds/0,
         to_integer/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec time_in_seconds() -> integer().
time_in_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())).

-spec to_integer(integer() | list() | binary()) -> integer().
to_integer(I) when is_integer(I)->
    I;
to_integer(L) when is_list(L)->
    list_to_integer(L);
to_integer(Bin) when is_binary(Bin)->
    list_to_integer(binary_to_list(Bin)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

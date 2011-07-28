%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(idioms).

%% API
-export([time_in_seconds/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec time_in_seconds() -> integer().
time_in_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())).

%%%===================================================================
%%% Internal functions
%%%===================================================================

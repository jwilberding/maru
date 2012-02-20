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
-export([format/1,
         format/2,
         log_info/3,
         log_warn/3,
         log_error/3,
         time_in_seconds/0,
         to_integer/1]).

%%%===================================================================
%%% API
%%%===================================================================
format(Any) ->
    format("~p", [Any]).

format(Format, Params) ->
    lists:flatten(io_lib:format(Format, Params)).

log_info(File, Line, Any)
  when is_atom(Any),
       is_list(Any) ->
    error_logger:info_msg(format("~s~n~s(~w):~n~s~n", [node(), File, Line, Any]));
log_info(File, Line, Any) ->
    error_logger:info_msg(format("~s~n~s(~w):~n~p~n", [node(), File, Line, Any])).

log_warn(File, Line, Any)
  when is_atom(Any),
       is_list(Any) ->
    error_logger:warning_msg(format("~s~n~s(~w):~n~s~n", [node(), File, Line, Any]));
log_warn(File, Line, Any) ->
    error_logger:warning_msg(format("~s~n~s(~w):~n~p~n", [node(), File, Line, Any])).

log_error(File, Line, Any)
  when is_atom(Any),
       is_list(Any) ->
    do_log_error(format("~s~n~s(~w):~n~s~n", [node(), File, Line, Any]));
log_error(File, Line, Any) ->
    do_log_error(format("~s~n~s(~w):~n~p~n", [node(), File, Line, Any])).

do_log_error(Error) ->
    error_logger:error_msg(Error).

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

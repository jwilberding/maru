%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_mail_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
start(_StartType, _StartArgs) ->
    case maru_mail_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%% @private
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

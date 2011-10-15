%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_mail).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 send/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {relay, username, password, port}).

%%%===================================================================
%%% Public Types
%%%===================================================================

send(From, To, Subject, Body) ->
    gen_server:cast(?SERVER, {send, From, To, Subject, Body}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, Relay} = application:get_env(relay),

    {ok, #state{relay=Relay}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({send, From, To, Subject, Body}, State=#state{relay=Relay}) ->
    Email = io_lib:format("Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]),
    gen_smtp_client:send({From, To, Email},
			 [{relay, Relay}]),
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

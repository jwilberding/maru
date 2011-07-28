%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_resource_authenticate).

%% API
-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("maru_web.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init(term()) -> {ok, record(ctx)}.
init(_) ->
    {ok, #ctx{}}.

-spec allowed_methods(term(), term()) -> tuple().
allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'POST'], ReqData, Ctx}.

-spec resource_exists(term(), term()) -> {true, term(), term()}.
resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

-spec process_post(term(), term()) -> {term(), term(), term()}.
process_post(ReqData, Ctx) ->
    case wrq:req_body(ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        JSON ->
            {struct, PropList} = mochijson2:decode(JSON),
            Username = proplists:get_value(<<"username">>, PropList),
            Password = bcrypt:hashpw(binary_to_list(proplists:get_value(<<"password">>, PropList)), bcrypt:gen_salt()),

            case maru_web_authenticate:is_valid(Username, Password) of
                true ->
                    NewReqData = maru_web_sessions:set_new_client_session_id(ReqData),
                    {true, NewReqData, Ctx};
                false ->
                    {false, ReqData, Ctx}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_web_base).

%% API
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         is_authorized/2,
         process_post/2,
         to_json/2,
         from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("maru_web/include/maru_web.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init(term()) -> {ok, record(ctx)}.
init(Model) ->
    {ok, #ctx{model=Model}}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

is_authorized(ReqData, Ctx) ->
    case maru_web_sessions:is_valid(ReqData) of
        true ->
            UserId = maru_web_sessions:get_user_id(ReqData),
            {true, ReqData, Ctx#ctx{user_id=UserId}};
        false ->
            {{halt, 302}, wrq:set_resp_header("Location", "/login", ReqData), Ctx}
    end.

process_post(ReqData, Ctx) ->
    case wrq:req_body(ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        JSON ->
            Model = (Ctx#ctx.model):to_record(JSON),
            case (Ctx#ctx.model):save(Model) of
                error ->
                    {false, ReqData, Ctx};
                _ ->
                    ReqData2 = wrq:append_to_response_body(JSON, ReqData),
                    {true, ReqData2, Ctx}
            end
    end.

to_json(ReqData, Ctx) ->
    case wrq:path_info(type, ReqData) of
        undefined ->
            Models = lists:foldr(fun(X, <<"">>) ->
                                         X;
                                    (X, Acc) ->
                                         <<Acc/binary, ",", X/binary>>
                                             end, <<"">>, [(Ctx#ctx.model):to_json(Model) || Model <- (Ctx#ctx.model):all()]),

            {<<"[", Models/binary, "]">>, ReqData, Ctx};
        ID ->
            case (Ctx#ctx.model):find({id, list_to_binary(ID)}) of
                not_found ->
                    {mochijson2:encode(null), ReqData, Ctx};
                [Model] ->
                    {(Ctx#ctx.model):to_json(Model), ReqData, Ctx}
            end
    end.

from_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {error, ReqData, Ctx};
        ID ->
            case (Ctx#ctx.model):find({id, ID}) of
                not_found ->
                    {error, ReqData, Ctx};
                OldModel ->
                    case wrq:req_body(ReqData) of
                        undefined ->
                            {error, ReqData, Ctx};
                        JSON ->
                            Model = (Ctx#ctx.model):to_record(JSON),
                            ID = (Ctx#ctx.model):get(id, OldModel),
                            (Ctx#ctx.model):save((Ctx#ctx.model):set([{id, ID}], Model)),
                            {<<"">>, ReqData, Ctx}
                    end
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
         process_post/2,
         to_json/2,
         to_html/2,
         from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("maru_web.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec init(term()) -> {ok, record(ctx)}.
init(Model) ->
    {ok, #ctx{model=Model}}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}, {"text/html", to_html}], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

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
            {mochijson2:encode(null), ReqData, Ctx};
        ID ->
            case (Ctx#ctx.model):find({id, ID}) of
                not_found ->
                    {mochijson2:encode(null), ReqData, Ctx};
                Model ->
                    {(Ctx#ctx.model):to_json(Model), ReqData, Ctx}
            end
    end.

to_html(ReqData, Ctx) ->
    case wrq:path_info(type, ReqData) of
        undefined ->
            {{halt, 404}, ReqData, Ctx};
        "new" ->
            maru_web_utils:return_file("new.html", ReqData, Ctx);
        "edit" ->
            maru_web_utils:return_file("edit.html", ReqData, Ctx);
        "show" ->
            maru_web_utils:return_file("show.html", ReqData, Ctx);
        _ ->
            {{halt, 404}, ReqData, Ctx}
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

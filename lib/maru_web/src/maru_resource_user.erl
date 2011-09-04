%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_resource_user).

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
init(_) ->
    {ok, #ctx{}}.

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
        UserJSON ->
            User = maru_model_users:to_record(UserJSON),
            case maru_model_users:save(User) of
                error ->
                    {false, ReqData, Ctx};
                _ ->
                    ReqData2 = wrq:append_to_response_body(UserJSON, ReqData),
                    {true, ReqData2, Ctx}
            end
    end.

to_json(ReqData, Ctx) ->
    case wrq:path_info(type, ReqData) of
        "check" ->
            {is_taken(username, ReqData), ReqData, Ctx};
        "email_check" ->
            {is_taken(email, ReqData), ReqData, Ctx};
        _ ->
            case wrq:path_info(type, ReqData) of
                undefined ->
                    {mochijson2:encode(null), ReqData, Ctx};
                Username ->
                    case maru_model_users:find({username, list_to_binary(Username)}) of
                        not_found ->
                            {mochijson2:encode(null), ReqData, Ctx};
                        User ->
                            {maru_model_users:to_json(User), ReqData, Ctx}
                    end
            end
    end.

to_html(ReqData, Ctx) ->
    {ok, {priv, App}} = application:get_env(host_dir),
    HostDir = code:priv_dir(App),
    NewCtx = Ctx#ctx{docroot=HostDir},
    case maru_web_utils:maybe_fetch_object(NewCtx, "user/new.html") of
        {true, NewCtx2} ->
            Body = NewCtx2#ctx.response_body,
            {Body, ReqData, NewCtx2};
        {false, NewCtx2} ->
            {error, ReqData, NewCtx2}
    end.

from_json(ReqData, Ctx) ->
    case wrq:path_info(username, ReqData) of
        undefined ->
            {error, ReqData, Ctx};
        Username ->
            case maru_model_users:find({username, list_to_binary(Username)}) of
                not_found ->
                    {error, ReqData, Ctx};
                OldUser ->
                    case wrq:req_body(ReqData) of
                        undefined ->
                            {error, ReqData, Ctx};
                        UserJSON ->
                            User = maru_model_users:to_record(UserJSON),
                            ID = maru_model_users:get(id, OldUser),
                            maru_model_users:save(maru_model_users:set([{id, ID}], User)),
                            {<<"">>, ReqData, Ctx}
                    end
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_taken(Attribute, ReqData) ->
    Value = wrq:get_qs_value(atom_to_list(Attribute), ReqData),
    case maru_model_users:find({Attribute, list_to_binary(Value)}) of
        not_found ->
            mochijson2:encode(true);
        _ ->
            mochijson2:encode(false)
    end.

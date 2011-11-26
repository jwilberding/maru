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
-include_lib("maru_web/include/maru_web.hrl").

-define(MODEL, maru_model_users).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(term()) -> {ok, record(ctx)}.
init(_) ->
    {ok, {priv, App}} = application:get_env(host_dir),
    HostDir = filename:join(code:priv_dir(App), "user"),
    {ok, #ctx{model=?MODEL, docroot=HostDir}}.

content_types_provided(ReqData, Ctx) ->
    maru_web_base:content_types_provided(ReqData, Ctx).

content_types_accepted(ReqData, Ctx) ->
    maru_web_base:content_types_accepted(ReqData, Ctx).

allowed_methods(ReqData, Ctx) ->
    maru_web_base:allowed_methods(ReqData, Ctx).

resource_exists(ReqData, Ctx) ->
    maru_web_base:resource_exists(ReqData, Ctx).

process_post(ReqData, Ctx) ->
    maru_web_base:process_post(ReqData, Ctx).

to_json(ReqData, Ctx) ->
    case wrq:path_info(type, ReqData) of
        "check" ->
            {is_taken(username, ReqData), ReqData, Ctx};
        "email_check" ->
            {is_taken(email, ReqData), ReqData, Ctx};
        _ ->
            case wrq:path_info(type, ReqData) of
                undefined ->
                    Models = [(Ctx#ctx.model):to_json(Model) || Model <- (Ctx#ctx.model):find([])],
                    {Models, ReqData, Ctx};
                Username ->
                    case maru_model_users:find({username, list_to_binary(Username)}) of
                        not_found ->
                            {mochijson2:encode(null), ReqData, Ctx};
                        [User] ->
                            NewUser = (Ctx#ctx.model):set([{password, <<"">>}], User),
                            {maru_model_users:to_json(NewUser), ReqData, Ctx}
                    end
            end
    end.

to_html(ReqData, Ctx) ->
    maru_web_base:to_html(ReqData, Ctx).

from_json(ReqData, Ctx) ->
    maru_web_base:from_json(ReqData, Ctx).

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

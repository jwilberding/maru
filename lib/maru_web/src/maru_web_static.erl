-module(maru_web_static).

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2]).

-include("maru_web.hrl").

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

init([]) ->
    {ok, HostDir} = application:get_env(host_dir),
    {ok, #ctx{docroot=HostDir}}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET'], ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Path = maru_web_utils:get_full_path(Ctx, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

provide_content(ReqData, Ctx) ->
    NewReqData = wrq:set_resp_header("X-Accel-Redirect", "/files/test.html", ReqData),
    {"", NewReqData, Ctx}.

    % case maru_web_utils:maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
    %     {true, NewCtx} ->
    %         Body = NewCtx#ctx.response_body,
    %         {Body, ReqData, Ctx};
    %     {false, NewCtx} ->
    %         {error, ReqData, NewCtx}
    % end.


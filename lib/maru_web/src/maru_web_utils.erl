%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_web_utils).

-include_lib("maru_web/include/maru_web.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([return_file/3,
         maybe_fetch_object/3,
         maybe_fetch_object/2,
         file_exists/2,
         get_full_path/2]).

%%%===================================================================
%%% API
%%%===================================================================
return_file(Filename, ReqData, Ctx) ->
    case maru_web_utils:maybe_fetch_object(Ctx, Filename) of
        {true, NewCtx} ->
            Body = NewCtx#ctx.response_body,
            {Body, ReqData, NewCtx};
        {false, NewCtx} ->
            {error, ReqData, NewCtx}
    end.


maybe_fetch_object(Ctx, Template, Path) ->
    % if returns {true, NewCtx} then NewCtx has response_body
    case Ctx#ctx.response_body of
        undefined ->
            case file_exists(Ctx, Path) of
                {true, FullPath} ->
                    Value = render_content(Template, FullPath),
                    {true, Ctx#ctx{response_body=Value}};
                false ->
                    {false, Ctx}
            end;
        _Body ->
            {true, Ctx}
    end.

maybe_fetch_object(Ctx, Path) ->
    % if returns {true, NewCtx} then NewCtx has response_body
    case Ctx#ctx.response_body of
        undefined ->
            case file_exists(Ctx, Path) of
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Ctx#ctx{response_body=Value}};
                false ->
                    {false, Ctx}
            end;
        _Body ->
            {true, Ctx}
    end.

file_exists(Ctx, Path) ->
    FPath = get_full_path(Ctx, Path),
    case filelib:is_regular(filename:absname(FPath)) of
        true ->
            {true, FPath};
        false ->
            false
    end.

get_full_path(Ctx, Path) ->
    Root = Ctx#ctx.docroot,
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> undefined;
        RelPath ->
            FullPath = filename:join([Root, RelPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath, "index.html"]);
                false ->
                    FullPath
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

render_content(Base, FullPath) ->
    {ok, Content} = file:read_file(FullPath),
    sgte:render(Base, [{content, Content}]).

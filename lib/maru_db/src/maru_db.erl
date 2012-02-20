%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_db).

-include_lib("maru_idioms/include/maru_idioms.hrl").

%% API
-export([all/1,
         all_keys/1,
         create_table/2,
         update/1,
         delete/2,
         store/1,
         find/2,
         match/2]).

%%%===================================================================
%%% API
%%%===================================================================

all(Tab) ->
    case find(Tab, []) of
        not_found ->
            [];
        All ->
            All
    end.

all_keys(Tab) ->
    case mnesia:transaction(?FUN(mnesia:all_keys(Tab))) of
        {atomic, Keys} ->
            Keys;
        {aborted, Reason} ->
            {error, Reason}
    end.

create_table(Name, Fields) ->
    mnesia:create_table(Name, [{disc_copies, [node()]}, {attributes, Fields}]).

% store(Record) when not(is_list(Record))->
%     store([Record]);
% store(Records) when is_list(Records)->
%     case mnesia:transaction(?FUN(lists:foreach(?FUN1(mnesia:write(X)), Records))) of
%         {atomic, ok} ->
%             ok;
%         _ ->
%             error
%     end.

store(Record) when not(is_list(Record))->
    store([Record]);
store(Records) when is_list(Records)->
    case mnesia:transaction(?FUN(lists:foreach(write_unique(_), Records))) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            ?NOTIFY_AIRBRAKE(error, Reason, Reason)
    end.

update(Record) when not(is_list(Record))->
    update([Record]);
update(Records) when is_list(Records)->
    case mnesia:transaction(?FUN(lists:foreach(mnesia:write(_), Records))) of
        {atomic, ok} ->
            ok;
        _ ->
            error
    end.

delete(Tab, Key) ->
    mnesia:dirty_delete(Tab, Key).

-spec find(atom(), list(tuple())) -> record().
find(Tab, PropList) ->
    Obj = Tab:new(match_spec(Tab, PropList)),
    case mnesia:transaction(
           ?FUN(mnesia:match_object(Obj))) of
        {atomic, []} ->
            not_found;
        {atomic, Result} ->
            Result;
        _ ->
            error
    end.

-spec match(atom(), list(tuple())) -> record().
match(Tab, PropList) ->
    Obj = Tab:new(match_spec(Tab, PropList)),
    case mnesia:transaction(
           ?FUN(mnesia:match_object(Obj))) of
        {atomic, []} ->
            not_found;
        {atomic, Result} ->
            Result;
        _ ->
            error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

match_spec(Tab, PropList) ->
    lists:ukeymerge(1, lists:keysort(1, PropList), lists:keysort(1, wildcard_match_spec(Tab))).

wildcard_match_spec(Tab) ->
    lists:map({_, '_'}, Tab:fields()).

write_unique(V) ->
    Tab = element(1, V),
    Key = element(2, V),
    case mnesia:wread({Tab, Key}) of
        [Exist] ->
            mnesia:abort(Exist);
        _ ->
            mnesia:write(V)
    end.

%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(maru_model_types).

%% API
-export([convert/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type maru_string() :: binary().
-type maru_key() :: binary().
-type maru_email() :: binary().
-type maru_password() :: list().
-type maru_address() :: binary().
-type maru_phone_number() :: binary().
-type maru_account_type() :: buyer | seller.
-type maru_account_claim_type() :: null.
-type maru_account_bankruptcy() :: null.
-type maru_account_jurisdiction() :: null.

-export_type([maru_string/0,
              maru_key/0,
              maru_email/0,
              maru_password/0,
              maru_address/0,
              maru_phone_number/0,
              maru_account_type/0,
              maru_account_claim_type/0,
              maru_account_bankruptcy/0,
              maru_account_jurisdiction/0]).

%%%===================================================================
%%% API
%%%===================================================================

convert(_Type, X='_') ->
    X;
convert(integer, X)  ->
    maru_idioms:to_integer(X);
convert(maru_string, X) when is_binary(X) ->
    X;
convert(maru_string, X) when is_list(X) ->
    list_to_binary(X);
convert(maru_password, X) when is_list(X) ->
    convert_maru_password(X);
convert(maru_password, X) when is_binary(X) ->
    convert_maru_password(X);
convert(_, X) ->
    X.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_maru_password(X) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, PW} = bcrypt:hashpw(X, Salt),
    PW.

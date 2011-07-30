%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is exprecs-0.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB.
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : exprecs.erl
%%% @author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% @end
%%% Description :
%%%
%%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------

-module(maru_model_transform).

-export([parse_transform/2,
         format_error/1,
         transform/3,
         context/2]).

-record(context, {module,
                  function,
                  arity}).

-record(pass1, {exports = [],
                record_types,
                generated = false,
                records = []}).

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),

            throw({error,get_pos(I),{unknown,R}})
        end).

get_pos(I) ->
    case proplists:get_value(form, I) of
        undefined ->
            0;
        Form ->
            erl_syntax:get_pos(Form)
    end.

parse_transform(Forms, Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try do_transform(Forms, Options) of
        Res ->
            %io:format("Res = ~p~n", [Res]),
            Res
    catch
        throw:{error, Ln, What} ->
            {error, [{File, [{Ln, ?MODULE, What}]}], []}
    end.

do_transform(Forms, _Options) ->
%%
%% 1st pass - collect record info
%%
    Fun1 =
        fun(attribute, {attribute, _L, record, RecDef}=Form, _Ctxt, Acc) ->
                Recs0 = Acc#pass1.records,
                Exports0 = Acc#pass1.exports,
                NewExports = Exports0 ++ [element(1, RecDef)],
                {Form, false, Acc#pass1{records = [RecDef|Recs0], exports = NewExports}};
           (attribute, {attribute, _L, type, {_, RecTypeDef, _}}=Form, _Ctxt, Acc) ->

                {Form, false, Acc#pass1{record_types=RecTypeDef}};
           (_Type, Form, _Context, Acc) ->
                {Form, false, Acc}
        end,

    {Forms1, Acc1} = pass(Forms, Fun1, _Acc = #pass1{}),
    %%
    %% 2nd pass - generate accessor functions
    %%
    Fun2 =
        fun(attribute, {attribute,L,record,_} = Form, _Ctxt,
            #pass1{exports = [_|_] = _Es} = Acc) ->
                Exports = [{get, 2},
                           {set, 2},
                           {new, 0},
                           {new, 1},
                           {fields, 0}],
                {[],
                 Form,
                 [{attribute,L,export,Exports}],
                 false, Acc};
           (function, Form, _Ctxt, #pass1{exports = [_|_],
                                          generated = false} = Acc) ->
                %% Layout record funs before first function
                L = element(2, Form),
                Funs = generate_accessors(L, Acc, Acc#pass1.record_types),
                {Funs, Form, [], false, Acc#pass1{generated = true}};
           (_Type, Form, _Ctxt, Acc) ->
                {Form, false, Acc}
        end,
    {Forms2, Acc2} = pass(Forms1, Fun2, Acc1),
    case Acc2#pass1.generated of
        true ->
            Forms2;
        false ->
            case Acc2#pass1.exports of
                [] ->
                    Forms2;
                [_|_] ->
                    [{eof,Last}|RevForms] = lists:reverse(Forms2),
                    [{function, NewLast, _, _, _}|_] = RevAs =
                        lists:reverse(generate_accessors(Last, Acc2, Acc2#pass1.record_types)),
                    lists:reverse([{eof, NewLast+1} | RevAs] ++ RevForms)
            end
    end.

pass(Forms, Fun, Acc) ->
    {NewTree, NewAcc} = transform(Forms, Fun, Acc),
    NewForms = [erl_syntax:revert(T) || T <- lists:flatten(NewTree)],
    {NewForms, NewAcc}.

generate_accessors(L, Acc, Types) ->
    AttrTypes = lists:map(
                  fun({typed_record_field,
                       {record_field, _L1, {atom, _L1, Attr}, _},
                       {remote_type, _L1, [_M, {atom, _L1, Type}, []]}}) ->
                          {Attr, Type};
                     ({typed_record_field,
                       {record_field, _L1, {atom, _L1, Attr}, _},
                       {type, _L1, T, _Args}}) ->
                          {Attr, T};
                     ({typed_record_field,
                       {record_field, _L1, {atom, _L1, Attr}},
                       {type, _L1, union,
                        [{atom, _L1, undefined},
                         {remote_type, _L1,
                          [{atom, _L1, _M}, {atom, _L1, T},[]]}]}}) ->
                          {Attr, T};
                     ({typed_record_field,
                       {record_field, _L1, {atom, _L1, Attr}},
                       {type, _L1, union,
                        [{atom, _L1, undefined},
                         {type, _L1, T, _Args}]}}) ->
                          {Attr, T};
                     ({typed_record_field,
                       {record_field, _L1, {atom, _L1, Attr}},
                       {type, _L1, union,
                        [{remote_type, _L1,
                          [{atom, _L1, _M}, {atom, _L1, T},[]]},
                        {atom, _L1, undefined}]}}) ->
                          {Attr, T}
                  end, Types),
    %io:format("AttrTypes ~p~n", [f_type_2(AttrTypes, L)]),
    lists:concat(lists:map(
                   fun(Rname) ->
                           Fields = get_flds(Rname, Acc),
                           [f_new_0(Rname, L),
                            f_new_1(Rname, L),
                            f_get_2(Rname, Fields, L),
                            f_set_2(Rname, Fields, L),
                            f_type_2(AttrTypes, L),
                            f_fields_0(Rname, L)]
                   end, Acc#pass1.exports)).

get_flds(Rname, #pass1{records = Rs}) ->
    {value, {_, Flds}} = lists:keysearch(Rname, 1, Rs),
    lists:map(
      fun({record_field,_, {atom,_,N}}) -> N;
         ({record_field,_, {atom,_,N}, _}) -> N
      end, Flds).

%%% Accessor functions
%%%
f_type_2(AttrTypes, L) ->
    {function, L, type, 1,
      [{clause, L, [{atom, L, Attr}], [],
        [{atom, L, Type}]} || {Attr, Type} <- AttrTypes]}.

f_new_0(Rname, L) ->
    {function, L, new, 0,
     [{clause, L, [], [],
       [{record, L, Rname, []}]}]}.


f_new_1(Rname, L) ->
    {function, L, new, 1,
     [{clause, L, [{var, L, 'Vals'}], [],
       [{call, L, {atom, L, set},
         [{var, L, 'Vals'},
          {record, L, Rname, []}
         ]}]
      }]}.

f_set_2(Rname, Flds, L) ->
    {function, L, set, 2,
     [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
       [{match, L, {var, L, 'F'},
         {'fun', L,
          {clauses,
           [{clause, L, [{nil,L},
                         {var,L,'R'},
                         {var,L,'_F1'}],
             [],
             [{var, L, 'R'}]} |
            [{clause, L,
              [{cons, L, {tuple, L, [{atom, L, Attr},
                                     {var,  L, 'V'}]},
                {var, L, 'T'}},
               {var, L, 'R'},
               {var, L, 'F1'}],
              [],
              [{call, L, {var, L, 'F1'},
                [{var,L,'T'},
                 {record, L, {var,L,'R'}, Rname,
                  [{record_field, L,
                    {atom, L, Attr},
                    {call, L, {remote, L,{atom, L, maru_model_types}, {atom, L, convert}}, [{call, L, {atom, L, type}, [{atom, L, Attr}]}, {var, L, 'V'}]}}]},
                 {var, L, 'F1'}]}]} || Attr <- Flds]]}}},
        {call, L, {var, L, 'F'}, [{var, L, 'Vals'},
                                  {var, L, 'Rec'},
                                  {var, L, 'F'}]}]}]}.

f_get_2(Rname, Flds, L) ->
    FName = get,
    {function, L, FName, 2,
     [{clause, L, [{var, L, 'Attrs'}, {var, L, 'R'}],
       [[{call, L, {atom, L, is_list}, [{var, L, 'Attrs'}]}]],
       [{lc, L, {call, L, {atom, L, FName}, [{var, L, 'A'}, {var, L, 'R'}]},
         [{generate, L, {var, L, 'A'}, {var, L, 'Attrs'}}]}]
      } |
      [{clause, L, [{atom, L, Attr}, {var, L, 'R'}], [],
        [{record_field, L, {var, L, 'R'}, Rname, {atom, L, Attr}}]} ||
          Attr <- Flds]]
    }.

f_fields_0(Rname, L) ->
    {function, L, fields, 0,
     [{clause, L, [], [],
       [{call, L, {atom, L, record_info},
         [{atom, L, fields}, {atom, L, Rname}]}]
      }]}.

%%% ========== generic parse_transform stuff ==============

context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A.


transform(Forms, F, Acc) ->
    case  [{L,M} || {attribute, L, module, M} <- Forms] of
	[{_,Module}] ->
	    transform(Forms, F, #context{module = Module}, Acc);
	[] ->
	    ?ERROR(missing_module_attribute, ?HERE, []);
	[_|_] = Multiple ->
	    ?ERROR(multiple_module_attributes, ?HERE,
		   [{L,{module,M}} || {L,M} <- Multiple])
    end.

transform(Forms, F, Context, Acc) ->
    F1 =
        fun(Form, Acc0) ->
                Type = erl_syntax:type(Form),
                {Before1, Form1, After1, Recurse, Acc1} =
                    try F(Type, Form, Context, Acc0) of
                        {F1, Rec1, A1} ->
                            {[], F1, [], Rec1, A1};
                        {_Be1, _F1, _Af1, _Rec1, _Ac1} = Res1 ->
                            Res1
                    catch
                        error:Reason ->
                            ?ERROR(Reason,
                                   ?HERE,
                                   [{type, Type},
                                    {context, Context},
                                    {acc, Acc},
                                    {form, Form}])
                    end,
                if Recurse == true ->
                        case erl_syntax:subtrees(Form1) of
                            [] ->
                                {Before1, Form1, After1, Acc1};
                            ListOfLists ->
                                {NewListOfLists, NewAcc} =
                                    mapfoldl(
                                      fun(L, AccX) ->
                                              transform(
                                                L, F,
                                                new_context(
                                                  Form1, Context), AccX)
                                      end, Acc1, ListOfLists),
                                NewForm =
                                    erl_syntax:update_tree(
                                      Form, NewListOfLists),
                                {Before1, NewForm, After1, NewAcc}
                        end;
                   true ->
                        {Before1, Form1, After1, Acc1}
                end
        end,
    mapfoldl(F1, Acc, Forms).


new_context(Form, Context0) ->
    case erl_syntax:type(Form) of
        function ->
            {Fun, Arity} =
                erl_syntax_lib:analyze_function(Form),
            Context0#context{function = Fun,
                             arity = Arity};
        _ ->
            Context0
    end.




%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {Before, Res, After, Accu1} =
	case F(Hd, Accu0) of
	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
		Result;
	    {R1, A1} ->
		{[], R1, [], A1}
	end,
    {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
    {Before ++ [Res| After ++ Rs], Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.



rpt_error(Reason, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n",
             "*** Location: ~p~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, Fun |
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).


format_error({_Cat, Error}) ->
    Error.

-module(ucumex_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/ucumex_parser.yrl", 26).

prefix_to_factor(<<"Y">>)  -> 1.0e24;
prefix_to_factor(<<"Z">>)  -> 1.0e21;
prefix_to_factor(<<"E">>)  -> 1.0e18;
prefix_to_factor(<<"P">>)  -> 1.0e15;
prefix_to_factor(<<"T">>)  -> 1.0e12;
prefix_to_factor(<<"G">>)  -> 1.0e9;
prefix_to_factor(<<"M">>)  -> 1.0e6;
prefix_to_factor(<<"k">>)  -> 1.0e3;
prefix_to_factor(<<"h">>)  -> 1.0e2;
prefix_to_factor(<<"da">>) -> 1.0e1;
prefix_to_factor(<<"d">>)  -> 1.0e-1;
prefix_to_factor(<<"c">>)  -> 1.0e-2;
prefix_to_factor(<<"m">>)  -> 1.0e-3;
prefix_to_factor(<<"u">>)  -> 1.0e-6;
prefix_to_factor(<<"n">>)  -> 1.0e-9;
prefix_to_factor(<<"p">>)  -> 1.0e-12;
prefix_to_factor(<<"f">>)  -> 1.0e-15;
prefix_to_factor(<<"a">>)  -> 1.0e-18;
prefix_to_factor(<<"z">>)  -> 1.0e-21;
prefix_to_factor(<<"y">>)  -> 1.0e-24;
prefix_to_factor(<<"Ki">>) -> 1024.0;
prefix_to_factor(<<"Mi">>) -> 1048576.0;
prefix_to_factor(<<"Gi">>) -> 1073741824.0;
prefix_to_factor(<<"Ti">>) -> 1099511627776.0.

with_bin_op({unit_exponent, {Unit, Exponent}}) ->
    {fun pow/2, Exponent, Unit};
with_bin_op({unit_exponent, {Prefix, Unit, Exponent}}) ->
    {fun pow/2, {prefix_to_factor(Prefix), Exponent}, Unit};
with_bin_op({prefix_atom_unit, {Prefix, Unit}}) ->
    {fun mul/2, prefix_to_factor(Prefix), Unit};
with_bin_op({atom_unit, Unit}) ->
    {fun mul/2, 1, Unit};
with_bin_op({factor, Factor}) ->
    {fun mul/2, 1, Factor}.

with_bin_op({solidus, <<"/">>}, Right) ->
    {fun divison/2, 1, Right}.

with_bin_op(Left, {period, <<".">>}, Right) ->
    {fun mul/2, Left, Right};
with_bin_op(Left, {solidus, <<"/">>}, Right) ->
    {fun divison/2, Left, Right}.

pow({Factor, Exponent}, {LowOp, X, [{U, E}]}) when is_function(LowOp) ->
    V = Factor * X,
    {LowOp, math:pow(V, Exponent), [{U, Exponent * E}]};
pow({_F, _E}=Term, {LO, X, U}) when is_binary(U) ->
    pow(Term, {LO, X, [{U, 1}]});
pow({LowOp, _X, _Unit}=TermA, {_Factor, _Exponent}=TermB) when is_function(LowOp) ->
    pow(TermB, TermA);
pow(Factor, {LowOp, _X, _Unit}=Term) when is_function(LowOp) ->
    pow({1, Factor}, Term);
pow({LowOp, _X, _Unit}=Term, Factor) when is_function(LowOp) ->
    pow({1, Factor}, Term);
pow({Factor, Exponent}, {X, [{U, E}]}) ->
    V = Factor * X,
    {math:pow(V, Exponent), [{U, E * Exponent}]};
pow({_F, _E}=Term, {X, U}) when is_binary(U) ->
    pow(Term, {X, [{U, 1}]});
pow({_Factor, _Exponent}=Term, Unit) ->
    pow(Term, {1, Unit});
pow(Exponent, {_X, _Unit}=Term) ->
    pow({1, Exponent}, Term);
pow(Exponent, X) ->
    math:pow(X, Exponent).

mul({VX, UX}, {VY, UY}) when is_list(UX) and is_list(UY) ->
    U = merge_mul_unit(UX, UY),
    {VX * VY, U};
mul({_VX, UX}=Term, {VY, UY}) when is_list(UX) ->
    mul(Term, {VY, [{UY, 1}]});
mul({VX, UX}, {_VY, UY}=Term) when is_list(UY) ->
    mul({VX, [{UX, 1}]}, Term);
mul({VX, UX}, {LowOp, VY, UY}) when is_function(LowOp) ->
    mul({VX, UX}, LowOp({VY, UY}));
mul({LowOp, VX, UX}, {VY, UY}) when is_function(LowOp) ->
    mul(LowOp({VX, UX}), {VY, UY});
mul({_VX, _UX}=Term, Factor) when is_number(Factor) ->
    mul(Term, {Factor, []});
mul(Factor, {_VX, _UX}=Term) ->
    mul({Factor, []}, Term);
mul(Factor, {LowOp, _V, _Unit}=Term) when is_function(LowOp) ->
    mul({Factor, []}, Term);
mul(Factor, Unit) when is_binary(Unit) ->
    mul({Factor, []}, {1, Unit});
mul(FactorA, FactorB) when is_number(FactorB) ->
    mul({FactorA, []}, {FactorB, []}).

divison({VX, UX}, {VY, UY}) when is_list(UX) and is_list(UY) ->
    U = merge_div_unit(UX, UY),
    {VX / VY, U};
divison({_VX, UX}=Term, {VY, UY}) when is_list(UX) ->
    divison(Term, {VY, [{UY, 1}]});
divison({VX, UX}, {_VY, UY}=Term) when is_list(UY) ->
    divison({VX, [{UX, 1}]}, Term);
divison(Factor, {_VX, _UX}=Term) when is_number(Factor) ->
    divison({Factor, []}, Term);
divison({_VX, _UX}=Term, Factor) when is_number(Factor) ->
    divison(Term, {Factor, []});
divison({_VX, _UX}=Term, {LowOp, VY, UY}) when is_function(LowOp) ->
    divison(Term, LowOp({VY, UY}));
divison({LowOp, VX, UX}, {_VY, _UY}=Term) when is_function(LowOp) ->
    divison(LowOp({VX, UX}), Term).

merge_mul_unit([], U) ->
    U;
merge_mul_unit(U, []) ->
    U;
merge_mul_unit([{K, V}|Rest], Acc) ->
    Acc2 = case lists:keyfind(K, 1, Acc) of
        {K, V2} ->
            lists:keyreplace(K, 1, Acc, {K, V + V2});
        false ->
            [{K, V} | Acc]
    end,
    merge_mul_unit(Rest, Acc2).

merge_div_unit([], U) ->
    lists:map(fun({K, V}) -> {K, -V} end, U);
merge_div_unit(UX, UY) ->
    UY2 = merge_div_unit([], UY),
    Op = fun({KX, VX}, Acc) ->
        case lists:keyfind(KX, 1, Acc) of
            {KX, VY} ->
                lists:keyreplace(KX, 1, Acc, {KX, VX + VY});
            false ->
                [{KX, VX} | Acc]
        end
    end,
    lists:foldl(Op, UY2, UX).

-file("/usr/local/Cellar/erlang/22.1.5/lib/erlang/lib/parsetools-2.1.8/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/ucumex_parser.erl", 309).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, solidus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(S, period, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, solidus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unit(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_annotatable(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_term(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_component(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_simple_unit(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_component(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, atom_unit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, factor, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, left_parent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, prefix_atom_unit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, unit_exponent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_simple_unit(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_10: see yeccpars2_8

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_annotatable(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(S, period, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, solidus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_unit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_13: see yeccpars2_8

%% yeccpars2_14: see yeccpars2_8

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_term(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_term(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
yeccpars2_17(S, period, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, right_parent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, solidus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_component(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_annotatable/7}).
yeccgoto_annotatable(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_annotatable(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_annotatable(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_annotatable(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_annotatable(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_component/7}).
yeccgoto_component(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_component(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_component(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_component(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_component(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_simple_unit/7}).
yeccgoto_simple_unit(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simple_unit(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simple_unit(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simple_unit(13=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simple_unit(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_term/7}).
yeccgoto_term(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_unit/7}).
yeccgoto_unit(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_6_/1}).
-file("src/ucumex_parser.yrl", 19).
yeccpars2_6_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/ucumex_parser.yrl", 13).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("src/ucumex_parser.yrl", 20).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("src/ucumex_parser.yrl", 16).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/ucumex_parser.yrl", 6).
yeccpars2_12_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/ucumex_parser.yrl", 9).
yeccpars2_15_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/ucumex_parser.yrl", 8).
yeccpars2_16_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   with_bin_op ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/ucumex_parser.yrl", 14).
yeccpars2_18_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("src/ucumex_parser.yrl", 159).

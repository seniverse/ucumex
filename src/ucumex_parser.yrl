Nonterminals
term unit component annotatable simple_unit.

Terminals
factor left_parent right_parent period solidus unit_exponent atom_unit prefix_atom_unit.

Rootsymbol unit.

unit -> term : '$1'.
unit -> solidus term : with_bin_op('$1', '$2').

term -> term period component : with_bin_op('$1', '$2', '$3').
term -> term solidus component : with_bin_op('$1', '$2', '$3').
term -> component : '$1'.

component -> annotatable : '$1'.
component -> factor : with_bin_op('$1').
component -> left_parent term right_parent : '$2'.

annotatable -> unit_exponent : with_bin_op('$1').
annotatable -> simple_unit : '$1'.

simple_unit -> atom_unit : with_bin_op('$1').
simple_unit -> prefix_atom_unit : with_bin_op('$1').

Erlang code.

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

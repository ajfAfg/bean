-module(prop_my_sets).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

set(Elem) -> {'$call', sets, from_list, [list(Elem)]}.

%%%%%%%%%%%%%%%%%%%%%
%%% my_sets:map/2 %%%
%%%%%%%%%%%%%%%%%%%%%
prop_map1(doc) -> "The first law of Functor Laws: Preservation of identity morphisms".

prop_map1() ->
    Id = fun(X) -> X end,
    ?FORALL(Set, set(any()), my_sets:equal(Set, my_sets:map(Id, Set))).

prop_map2(doc) ->
    "The second law of Functor Laws: Preservation of composition of morphisms".

prop_map2() ->
    Comp = fun(F, G) -> fun(X) -> F(G(X)) end end,
    S = random_type(),
    T = random_type(),
    U = random_type(),
    ?FORALL({Set, F, G},
            {set(S), function([T], U), function([S], T)},
            my_sets:equal(
                my_sets:map(Comp(F, G), Set), my_sets:map(F, my_sets:map(G, Set)))).

%%%%%%%%%%%%%%%%%%%%%
%%% my_sets:any/2 %%%
%%%%%%%%%%%%%%%%%%%%%
prop_any1(doc) -> "Condition `my_sets:any/2` returns `true`".

prop_any1() ->
    T = random_type(),
    ?FORALL({Set, Pred},
            {set(T), function([T], boolean())},
            sets:fold(fun(X, Acc) -> Pred(X) orelse Acc end, false, Set)
            =:= my_sets:any(Pred, Set)).

prop_any2(doc) -> "Condition `my_sets:any/2` returns `false`".

prop_any2() ->
    T = random_type(),
    ?FORALL({Set, Pred},
            {set(T), function([T], boolean())},
            ?IMPLIES(lists:all(fun(X) -> not Pred(X) end, sets:to_list(Set)),
                     not my_sets:any(Pred, Set))).

%%%%%%%%%%%%%%%%%%%%%%%
%%% my_sets:equal/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% It was difficult to show the properties of the following equation due to technical problems.
%
% - Symmetric relation (For all s1, s2: sets:set(T). my_sets:equal(s1, s2) -> my_sets:equal(s2, s1))
%   - Not much data is generated that satisfies the property
% - Transitive relation (For all s1, s2, s3: sets:set(T). my_sets:equal(s1, s2) /\ my_sets:equal(s2, s3) -> my_sets:equal(s1, s3))
%   - Not much data is generated that satisfies the property
% - Substitution law (For all s1, s2: sets:set(T), p: T -> boolean(). p(s1) /\ my_sets:equal(s1, s2) -> p(s2))
%   - `my_sets:equal/2` and `=:=` have different meanings
%   - i.e. Even if my_sets:equal(s1, s2) is true, p(s1) and p(s2) are not necessarily both true
%   - e.g. `sets:from_list([true,false]) =/= sets:from_list([false,true])`
prop_equal(doc) -> "Wording the specification differently".

prop_equal() ->
    T = random_type(),
    Equal =
        fun(Set1, Set2) ->
           {S1, S2} =
               case sets:size(Set1) >= sets:size(Set2) of
                   true -> {Set1, Set2};
                   false -> {Set2, Set1}
               end,
           sets:to_list(
               sets:subtract(S1, S2))
           =:= []
        end,
    ?FORALL({Set1, Set2}, {set(T), set(T)}, Equal(Set1, Set2) =:= my_sets:equal(Set1, Set2)).

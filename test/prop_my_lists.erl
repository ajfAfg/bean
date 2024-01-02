-module(prop_my_lists).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:shuffle/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_shuffle1(doc) -> "Elements of lists are the same".

prop_shuffle1() ->
    ?FORALL(List,
            list(random_type()),
            lists:sort(List)
            =:= lists:sort(
                    my_lists:shuffle(List))).

% NOTE:
% It is possible that this test will fail because a list before and after shuffling happen to be equal,
% but this is acceptable.
prop_shuffle2(doc) -> "In many case the order of elements is different".

prop_shuffle2() ->
    ?FORALL(List, vector(100, random_type()), List =/= my_lists:shuffle(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:sublist_randomly/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_sublist_randomly1(doc) -> "The return value is smaller than the original list".

prop_sublist_randomly1() ->
    ?FORALL(List,
            list(random_type()),
            length(my_lists:sublist_randomly(List)) =< length(List)).

prop_sublist_randomly2(doc) ->
    "All of the elements of the return value are included in the original list".

prop_sublist_randomly2() ->
    ?FORALL(List, list(random_type()), my_lists:sublist_randomly(List) -- List =:= []).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:power/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%
prop_power1(doc) ->
    "Let n be the length of the original list, the length of the return value is 2^n".

prop_power1() ->
    ?FORALL(List,
            ?SUCHTHAT(L, list(random_type()), length(L) =< 20),
            length(my_lists:power(List)) =:= trunc(math:pow(2, length(List)))).

prop_power2(doc) -> "The element of the return value is a sublist of the original list".

prop_power2() ->
    ?FORALL(List,
            ?SUCHTHAT(L, list(random_type()), length(L) =< 15),
            lists:all(fun(List2) -> lists:sort(List2 ++ List -- List2) =:= lists:sort(List) end,
                      my_lists:power(List))).

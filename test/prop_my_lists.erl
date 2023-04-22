-module(prop_my_lists).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:shuffle/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_shuffle1(doc) -> "Elements of lists are the same".

prop_shuffle1() ->
    ?FORALL(List,
            list(any()),
            lists:sort(List)
            =:= lists:sort(
                    my_lists:shuffle(List))).

% NOTE:
% It is possible that this test will fail because a list before and after shuffling happen to be equal,
% but this is acceptable.
prop_shuffle2(doc) -> "In many case the order of elements is different".

prop_shuffle2() -> ?FORALL(List, vector(100, any()), List =/= my_lists:shuffle(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:sublist_randomly/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_sublist_randomly1(doc) -> "The return value is smaller than the original list".

prop_sublist_randomly1() ->
    ?FORALL(List, list(), length(my_lists:sublist_randomly(List)) =< length(List)).

prop_sublist_randomly2(doc) ->
    "All of the elements of the return value are included in the original list".

prop_sublist_randomly2() ->
    ?FORALL(List, list(), my_lists:sublist_randomly(List) -- List =:= []).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:search_index/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_search_index(doc) -> "Inverse of `lists:nth/2`".

prop_search_index() ->
    ?FORALL(List,
            non_empty(list(random_type())),
            begin
                lists:all(fun(X) -> X end,
                          [lists:nth(
                               my_lists:search_index(Elem, List), List)
                           =:= Elem
                           || Elem <- List])
            end).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_lists:foldl/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%
prop_foldl(doc) -> "Fold with the head of the given list as the initial value".

prop_foldl() ->
    T = random_type(),
    ?FORALL({List, Fun},
            {non_empty(list(T)), function([T, T], T)},
            lists:foldl(Fun, hd(List), tl(List)) =:= my_lists:foldl(Fun, List)).

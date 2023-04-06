-module(my_sets).

-export([map/2, any/2, equal/2]).

-spec map(fun((A) -> B), sets:set(A)) -> sets:set(B)
    when A :: term(),
         B :: term().
map(Fun, Set) ->
    sets:from_list(
        lists:map(Fun, sets:to_list(Set))).

-spec any(Pred, Set) -> boolean()
    when Pred :: fun((Elem :: T) -> boolean()),
         Set :: sets:set(T),
         T :: term().
any(Pred, Set) -> lists:any(Pred, sets:to_list(Set)).

-spec equal(Set, Set) -> boolean() when Set :: sets:set(term()).
equal(Set1, Set2) ->
    lists:sort(
        sets:to_list(Set1))
    =:= lists:sort(
            sets:to_list(Set2)).

-module(my_sets).

-export([map/2, any/2]).

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

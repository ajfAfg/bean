-module(my_lists).

-export([shuffle/1, sublist_randomly/1, search_index/2, foldl/2]).

-spec shuffle(list()) -> list().
shuffle(List) -> [X || {_, X} <- lists:sort([{rand:uniform(), Y} || Y <- List])].

-spec sublist_randomly(list()) -> list().
sublist_randomly(List) ->
    lists:sublist(
        my_lists:shuffle(List), rand:uniform(length(List) + 1) - 1).

-spec search_index(term(), list()) -> pos_integer().
search_index(Elem, List) ->
    case [V || V <- lists:enumerate(List), {_I, E} <- [V], E =:= Elem] of
        [] -> false;
        [{I, Elem} | _] -> I
    end.

-spec foldl(Fun, List) -> Acc
    when Fun :: fun((Elem :: T, AccIn) -> AccOut),
         Acc :: term(),
         AccIn :: term(),
         AccOut :: term(),
         List :: [T],
         T :: term().
foldl(F, [Head | Rest]) -> lists:foldl(F, Head, Rest).

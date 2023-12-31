-module(my_lists).

-export([shuffle/1, sublist_randomly/1]).

-spec shuffle(list()) -> list().
shuffle(List) -> [X || {_, X} <- lists:sort([{rand:uniform(), Y} || Y <- List])].

-spec sublist_randomly(list()) -> list().
sublist_randomly(List) ->
    lists:sublist(
        my_lists:shuffle(List), rand:uniform(length(List) + 1) - 1).

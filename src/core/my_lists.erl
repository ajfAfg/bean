-module(my_lists).

-export([shuffle/1, sublist_randomly/1, power/1, flatten/1]).

-spec shuffle(list()) -> list().
shuffle(List) -> [X || {_, X} <- lists:sort([{rand:uniform(), Y} || Y <- List])].

-spec sublist_randomly(list()) -> list().
sublist_randomly(List) ->
    lists:sublist(
        my_lists:shuffle(List), rand:uniform(length(List) + 1) - 1).

-spec power(list()) -> [list()].
power(List) -> power_([], List, []).

power_(Subset, [], Family) -> [Subset | Family];
power_(Subset, [Head | Rest], Family) ->
    power_(Subset, Rest, power_([Head | Subset], Rest, Family)).

% NOTE: Unlike `lists:flatten/1`, this function removes only one level of the nested list.
-spec flatten([list()]) -> list().
flatten(ListList) -> [X || List <- ListList, X <- List].

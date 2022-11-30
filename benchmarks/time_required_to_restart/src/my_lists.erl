-module(my_lists).

-export([shuffle/1]).

% NOTE:
% Refer https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-randomly-re-arrange-list-elements
-spec shuffle(list()) -> list().
shuffle(List) -> [Y || {_, Y} <- lists:sort([{rand:uniform(), X} || X <- List])].

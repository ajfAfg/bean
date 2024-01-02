-module(dependency_graph_generator).

-export([dependency_graph/0]).

-import(proper_helper, [limited_atom/0]).

-include_lib("proper/include/proper.hrl").

dependency_graph() ->
    ?LET(List,
         non_empty(list(limited_atom())),
         begin
             Vertices = lists:uniq(List),
             lists:foldl(fun(From, Acc) ->
                            Tos = my_lists:sublist_randomly(Vertices),
                            maps:put(From, Tos, Acc)
                         end,
                         #{},
                         Vertices)
         end).

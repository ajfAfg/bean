-module(my_digraph_tests).

-include_lib("eunit/include/eunit.hrl").

create_test_() ->
    {inparallel,
     [{"Return a correct graph",
       fun() ->
          Vertices = [1, 2, 3, 4],
          Edges = [{1, 2}, {1, 3}, {2, 4}, {3, 4}],
          G = my_digraph:create(Vertices, Edges),
          ?assertEqual(lists:sort(Vertices),
                       lists:sort(
                           digraph:vertices(G))),
          ?assertEqual(lists:sort(Edges),
                       lists:sort(
                           lists:map(fun(E) ->
                                        {E, V1, V2, _} = digraph:edge(G, E),
                                        {V1, V2}
                                     end,
                                     digraph:edges(G))))
       end}]}.

has_path_test_() ->
    {inparallel,
     [{"Return `true` if the given graph has a path",
       ?_assert(my_digraph:has_path(
                    my_digraph:create([1, 2], [{1, 2}]), 1, 2))},
      {"Return `false` if the given graph has no path",
       ?_assertNot(my_digraph:has_path(
                       my_digraph:create([1, 2], []), 1, 2))}]}.

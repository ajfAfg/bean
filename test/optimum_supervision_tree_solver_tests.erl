-module(optimum_supervision_tree_solver_tests).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [map/2, sort/1]).

% NOTE:
% Only test for types,
% since `group/1` and `transform_into_optimum_supervision_tree/1` tests are enriched.
solve_test_() ->
    {inparallel,
     [{"Can run if the input type matches",
       ?_assertMatch({_, [_ | _]},
                     optimum_supervision_tree_solver:solve(#{g1 => [g2], g2 => []}))}]}.

% TODO: Use helper functions to reduce the amount of code.
group_test_() ->
    ExtractEdges =
        fun(Graph) ->
           lists:map(fun(E) ->
                        {E, V1, V2, _} = digraph:edge(Graph, E),
                        {sort(V1), sort(V2)}
                     end,
                     digraph:edges(Graph))
        end,
    {inparallel,
     [{"Return a different instance from the given graph",
       fun() ->
          G = digraph:new(),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertNotEqual(G, GroupedGraph)
       end},
      {"Return grouped vertices",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 7)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {2, 3}, {3, 4}, {3, 5}, {6, 7}]),
          ?assertEqual(sort(map(fun lists:sort/1, [[1], [2], [3], [4, 5], [6], [7]])),
                       sort(map(fun lists:sort/1,
                                digraph:vertices(
                                    optimum_supervision_tree_solver:group(G)))))
       end},
      {"Keep the original edges",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 7)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {2, 3}, {3, 4}, {3, 5}, {6, 7}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort([{[1], [3]}, {[2], [3]}, {[3], sort([4, 5])}, {[6], [7]}]),
                       sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph like a lattice",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 6)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort(map(fun lists:sort/1, [[6], [3, 4, 5], [1], [2]])),
                       sort(map(fun lists:sort/1, digraph:vertices(GroupedGraph)))),
          ?assertEqual(lists:sort([{[1], sort([3, 4, 5])},
                                   {[2], sort([3, 4, 5])},
                                   {sort([3, 4, 5]), [6]}]),
                       lists:sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph like a binary tree",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 7)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {2, 3}, {3, 7}, {4, 6}, {5, 6}, {6, 7}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort([[1], [2], [3], [4], [5], [6], [7]]),
                       sort(digraph:vertices(GroupedGraph))),
          ?assertEqual(sort([{[1], [3]},
                             {[2], [3]},
                             {[3], [7]},
                             {[4], [6]},
                             {[5], [6]},
                             {[6], [7]}]),
                       sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph having a circle",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 5), [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {3, 5}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort(map(fun lists:sort/1, [[1], [2, 3, 4], [5]])),
                       sort(map(fun lists:sort/1, digraph:vertices(GroupedGraph)))),
          ?assertEqual(sort([{[1], sort([2, 3, 4])}, {sort([2, 3, 4]), [5]}]),
                       ExtractEdges(GroupedGraph))
       end},
      {"Can group a graph having circles in first and last",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create(
                      lists:seq(1, 6), [{1, 2}, {2, 3}, {3, 1}, {3, 4}, {4, 5}, {5, 6}, {6, 5}])),
          ?assertEqual(sort(map(fun lists:sort/1, [[1, 2, 3], [4], [5, 6]])),
                       sort(map(fun lists:sort/1, digraph:vertices(GroupedGraph)))),
          ?assertEqual(sort([{sort([1, 2, 3]), [4]}, {[4], sort([5, 6])}]),
                       sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph having strongly connected components",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create([1, 2, 3, 4], [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}])),
          ?assertEqual([sort([1, 2, 3, 4])], map(fun lists:sort/1, digraph:vertices(GroupedGraph))),
          ?assertEqual([], ExtractEdges(GroupedGraph))
       end},
      {"Can group a complex graph",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 4), [{1, 2}, {1, 4}, {2, 3}, {3, 4}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort([[3, 2, 1], [4]]), sort(digraph:vertices(GroupedGraph))),
          % NOTE: Keep the order of `[3, 2, 1]`
          ?assertEqual([{[3, 2, 1], [4]}],
                       sort(map(fun(E) ->
                                   {E, V1, V2, _} = digraph:edge(GroupedGraph, E),
                                   {V1, V2}
                                end,
                                digraph:edges(GroupedGraph))))
       end},
      {"Group each vertex individually if no edges exist",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 3), []),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(sort([[1], [2], [3]]), sort(digraph:vertices(GroupedGraph))),
          ?assertEqual([], ExtractEdges(GroupedGraph))
       end},
      {"Label cyclic strongly connected components as `cyclic_strong_component`",
       fun() ->
          ?assertMatch({_, cyclic_strong_component},
                       digraph:vertex(
                           optimum_supervision_tree_solver:group(
                               my_digraph:create([1, 2], [{1, 2}, {2, 1}])),
                           [2, 1])),
          ?assertMatch({_, cyclic_strong_component},
                       digraph:vertex(
                           optimum_supervision_tree_solver:group(
                               my_digraph:create([1, 2, 3, 4],
                                                 [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}])),
                           [4, 3, 2, 1]))
       end},
      {"Does not label singletons as `cyclic_strong_component`",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create([1, 2], [])),
          ?assertNotMatch({[1], cyclic_strong_component}, digraph:vertex(GroupedGraph, [1])),
          ?assertNotMatch({[2], cyclic_strong_component}, digraph:vertex(GroupedGraph, [2]))
       end},
      {"If a vertex belonging to a strongly connected component consisting of multiple vertices is present in the group, it is monitored by one_for_all.",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create(
                      lists:seq(1, 6), [{1, 2}, {2, 3}, {3, 1}, {2, 4}, {2, 6}, {6, 4}])),
          ?assertEqual(sort([[6, 3, 2, 1], [4], [5]]), sort(digraph:vertices(GroupedGraph))),
          ?assertEqual([{[1, 2, 3, 6], [4]}], ExtractEdges(GroupedGraph)),
          ?assertMatch({_, cyclic_strong_component}, digraph:vertex(GroupedGraph, [6, 3, 2, 1]))
       end}]}.

sort_by_postorder_test_() ->
    {inparallel,
     [{"Sort by the postorder of the vertices",
       fun() ->
          G = my_digraph:create([1, 2, 3], [{1, 2}, {2, 3}]),
          ?assertEqual([3, 2, 1], optimum_supervision_tree_solver:sort_by_postorder([2, 3, 1], G))
       end},
      {"Return the same result as reversed topological sorting if the given list is a DAG",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 6), [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          ?assertEqual(lists:reverse(
                           digraph_utils:topsort(G)),
                       optimum_supervision_tree_solver:sort_by_postorder(
                           lists:seq(1, 6), G))
       end},
      {"Can sort if the given graph has cycles",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 5), [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {3, 5}]),
          ?assertEqual([5, 4, 3, 2, 1],
                       optimum_supervision_tree_solver:sort_by_postorder(
                           lists:seq(1, 5), G))
       end}]}.

transform_into_optimum_supervision_tree_test_() ->
    {inparallel,
     [{"Dependency-free behaviors are monitored by `one_for_one`.",
       fun() ->
          GroupedGraph =
              my_digraph:create([[1], [2], [3], [4]], [{[1], [4]}, {[2], [4]}, {[3], [4]}]),
          ?assertEqual({rest_for_one,
                        [4,
                         {one_for_one,
                          % NOTE: Not optimal yet.
                          [{rest_for_one, [3]}, {rest_for_one, [2]}, {rest_for_one, [1]}]}]},
                       optimum_supervision_tree_solver:transform_into_optimum_supervision_tree(GroupedGraph))
       end},
      {"Interdependent behaviors are monitored by `one_for_all`.",
       fun() ->
          GroupedGraph = my_digraph:create([[1, 2, 3]], []),
          digraph:add_vertex(GroupedGraph, [1, 2, 3], cyclic_strong_component),
          ?assertEqual({one_for_all, [1, 2, 3]},
                       optimum_supervision_tree_solver:transform_into_optimum_supervision_tree(GroupedGraph))
       end},
      {"`GroupedVertex` with an in-degree of 0 and a length greater than or equal to 2 is monitored by `rest_for_one`.",
       fun() ->
          GroupedGraph = my_digraph:create([[1, 2, 3], [4]], [{[1, 2, 3], [4]}]),
          ?assertEqual({rest_for_one, [4, {rest_for_one, [1, 2, 3]}]},
                       optimum_supervision_tree_solver:transform_into_optimum_supervision_tree(GroupedGraph))
       end}]}.

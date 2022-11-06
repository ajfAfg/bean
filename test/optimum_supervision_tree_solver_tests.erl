-module(optimum_supervision_tree_solver_tests).

-include_lib("eunit/include/eunit.hrl").

% TODO: Use helper functions to reduce the amount of code.
group_test_() ->
    ExtractEdges =
        fun(Graph) ->
           lists:map(fun(E) ->
                        {E, V1, V2, _} = digraph:edge(Graph, E),
                        {V1, V2}
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
          ?assertEqual(lists:sort([[1], [2], [3], [4, 5], [6], [7]]),
                       lists:sort(
                           digraph:vertices(
                               optimum_supervision_tree_solver:group(G))))
       end},
      {"Keep the original edges",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 7)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {2, 3}, {3, 4}, {3, 5}, {6, 7}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([{[1], [3]}, {[2], [3]}, {[3], [4, 5]}, {[6], [7]}]),
                       lists:sort(
                           lists:map(fun(E) ->
                                        {E, V1, V2, _} = digraph:edge(GroupedGraph, E),
                                        {V1, V2}
                                     end,
                                     digraph:edges(GroupedGraph))))
       end},
      {"Can group a graph like a lattice",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 6)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([[6], [3, 4, 5], [1], [2]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual(lists:sort([{[1], [3, 4, 5]}, {[2], [3, 4, 5]}, {[3, 4, 5], [6]}]),
                       lists:sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph like a binary tree",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, lists:seq(1, 7)),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{1, 3}, {2, 3}, {3, 7}, {4, 6}, {5, 6}, {6, 7}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([[1], [2], [3], [4], [5], [6], [7]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual(lists:sort([{[1], [3]},
                                   {[2], [3]},
                                   {[3], [7]},
                                   {[4], [6]},
                                   {[5], [6]},
                                   {[6], [7]}]),
                       lists:sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph having a circle",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 5), [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {3, 5}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([[1], [2, 3, 4], [5]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual(lists:sort([{[1], [2, 3, 4]}, {[2, 3, 4], [5]}]), ExtractEdges(GroupedGraph))
       end},
      {"Can group a graph having circles in first and last",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create(
                      lists:seq(1, 6), [{1, 2}, {2, 3}, {3, 1}, {3, 4}, {4, 5}, {5, 6}, {6, 5}])),
          ?assertEqual(lists:sort([[1, 2, 3], [4], [5, 6]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual(lists:sort([{[1, 2, 3], [4]}, {[4], [5, 6]}]),
                       lists:sort(ExtractEdges(GroupedGraph)))
       end},
      {"Can group a graph having strongly connected components",
       fun() ->
          GroupedGraph =
              optimum_supervision_tree_solver:group(
                  my_digraph:create([1, 2, 3, 4], [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}])),
          ?assertEqual([[1, 2, 3, 4]], digraph:vertices(GroupedGraph)),
          ?assertEqual([], ExtractEdges(GroupedGraph))
       end},
      {"Can group a complex graph",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 4), [{1, 2}, {1, 4}, {2, 3}, {3, 4}]),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([[1, 2, 3], [4]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual(lists:sort([{[1, 2, 3], [4]}]), lists:sort(ExtractEdges(GroupedGraph)))
       end},
      {"Group each vertex individually if no edges exist",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 3), []),
          GroupedGraph = optimum_supervision_tree_solver:group(G),
          ?assertEqual(lists:sort([[1], [2], [3]]),
                       lists:sort(
                           digraph:vertices(GroupedGraph))),
          ?assertEqual([], ExtractEdges(GroupedGraph))
       end}]}.

sort_by_postorder_test_() ->
    {inparallel,
     [{"Sort by the postorder of the vertices",
       fun() ->
          G = my_digraph:create([1, 2, 3], [{1, 2}, {2, 3}]),
          ?assertEqual([1, 2, 3], optimum_supervision_tree_solver:sort_by_postorder([2, 3, 1], G))
       end},
      {"Return the same result as topological sorting if the given list is a DAG",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 6), [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          ?assertEqual(digraph_utils:topsort(G),
                       optimum_supervision_tree_solver:sort_by_postorder(
                           lists:seq(1, 6), G))
       end},
      {"Can sort if the given graph has cycles",
       fun() ->
          G = my_digraph:create(
                  lists:seq(1, 5), [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {3, 5}]),
          ?assertEqual([1, 2, 3, 4, 5],
                       optimum_supervision_tree_solver:sort_by_postorder(
                           lists:seq(1, 5), G))
       end}]}.

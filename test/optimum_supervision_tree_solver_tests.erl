-module(optimum_supervision_tree_solver_tests).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [map/2, sort/1]).

% NOTE: Tests for critical inputs.
solve_test_() ->
    CreateGraph =
        fun(Vertices, Edges) ->
           lists:foldl(fun({From, To}, Map) ->
                          maps:update_with(From, fun(Tos) -> [To | Tos] end, Map)
                       end,
                       maps:from_keys(Vertices, []),
                       Edges)
        end,
    {inparallel,
     [{"Standard graph",
       fun() ->
          G = CreateGraph(lists:seq(1, 7), [{1, 2}, {2, 3}, {3, 4}, {3, 5}, {6, 7}]),
          ?assertEqual({one_for_one,
                        [{rest_for_one,
                          [5, 4, {rest_for_one, [3, {rest_for_one, [2, {rest_for_one, [1]}]}]}]},
                         {rest_for_one, [7, {rest_for_one, [6]}]}]},
                       optimum_supervision_tree_solver:solve(G))
       end},
      {"Graph like Lattice",
       fun() ->
          G = CreateGraph(lists:seq(1, 6),
                          [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          ?assertEqual({rest_for_one,
                        [6,
                         {rest_for_one,
                          [5, 3, 4, {one_for_one, [{rest_for_one, [1]}, {rest_for_one, [2]}]}]}]},
                       optimum_supervision_tree_solver:solve(G))
       end},
      {"Binary tree",
       fun() ->
          G = CreateGraph(lists:seq(1, 7), [{1, 3}, {2, 3}, {3, 7}, {4, 6}, {5, 6}, {6, 7}]),
          ?assertEqual({rest_for_one,
                        [7,
                         {one_for_one,
                          [{rest_for_one,
                            [6, {one_for_one, [{rest_for_one, [5]}, {rest_for_one, [4]}]}]},
                           {rest_for_one,
                            [3, {one_for_one, [{rest_for_one, [1]}, {rest_for_one, [2]}]}]}]}]},
                       optimum_supervision_tree_solver:solve(G))
       end},
      {"Graph having a circle",
       fun() ->
          G1 = CreateGraph(lists:seq(1, 5), [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {3, 5}]),
          ?assertEqual({rest_for_one, [5, {one_for_all, [4, 3, 2, {rest_for_one, [1]}]}]},
                       optimum_supervision_tree_solver:solve(G1)),
          G2 = CreateGraph(lists:seq(1, 5), [{1, 2}, {2, 3}, {2, 4}, {2, 5}, {3, 1}, {4, 5}]),
          ?assertEqual({rest_for_one, [5, {one_for_all, [3, 4, 2, 1]}]},
                       optimum_supervision_tree_solver:solve(G2))
       end},
      {"Graph having circles in first and last",
       fun() ->
          G = CreateGraph(lists:seq(1, 6),
                          [{1, 2}, {2, 3}, {3, 1}, {3, 4}, {4, 5}, {5, 6}, {6, 5}]),
          ?assertEqual({one_for_all, [6, 5, {rest_for_one, [4, {one_for_all, [3, 2, 1]}]}]},
                       optimum_supervision_tree_solver:solve(G))
       end},
      {"Cycle graph",
       fun() ->
          G = CreateGraph([1, 2, 3, 4], [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}]),
          ?assertEqual({one_for_all, [3, 4, 2, 1]}, optimum_supervision_tree_solver:solve(G))
       end},
      {"Graph with detour",
       fun() ->
          G = CreateGraph(lists:seq(1, 4), [{1, 2}, {1, 4}, {2, 3}, {3, 4}]),
          ?assertEqual({rest_for_one, [4, {rest_for_one, [3, 2, 1]}]},
                       optimum_supervision_tree_solver:solve(G))
       end},
      {"Null graph",
       fun() ->
          G = CreateGraph(lists:seq(1, 3), []),
          ?assertEqual({one_for_one,
                        [{rest_for_one, [2]}, {rest_for_one, [1]}, {rest_for_one, [3]}]},
                       optimum_supervision_tree_solver:solve(G))
       end}]}.

group_test_() ->
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
          ?assertEqual(sets:from_list([sets:from_list(V)
                                       || V <- [[1], [2], [3], [4, 5], [6], [7]]]),
                       sets:from_list(
                           digraph:vertices(
                               optimum_supervision_tree_solver:group(G))))
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

transform_test_() ->
    CreateGraph =
        fun(Vertices, Edges) ->
           NewVertices = [sets:from_list(V) || V <- Vertices],
           NewEdges = [{sets:from_list(From), sets:from_list(To)} || {From, To} <- Edges],
           my_digraph:create(NewVertices, NewEdges)
        end,
    {inparallel,
     [{"Dependency-free behaviors are monitored by `one_for_one`.",
       fun() ->
          Graph = my_digraph:create([1, 2, 3, 4], [{1, 4}, {2, 4}, {3, 4}]),
          GroupedGraph = CreateGraph([[1], [2], [3], [4]], [{[1], [4]}, {[2], [4]}, {[3], [4]}]),
          ?assertEqual({rest_for_one,
                        [4,
                         {one_for_one,
                          % NOTE: Not optimal yet.
                          [{rest_for_one, [3]}, {rest_for_one, [2]}, {rest_for_one, [1]}]}]},
                       optimum_supervision_tree_solver:transform(Graph, GroupedGraph))
       end},
      {"Interdependent behaviors are monitored by `one_for_all`.",
       fun() ->
          Graph1 = my_digraph:create([1, 2], [{1, 2}, {2, 1}]),
          GroupedGraph1 = CreateGraph([[1, 2]], []),
          ?assertEqual({one_for_all, [2, 1]},
                       optimum_supervision_tree_solver:transform(Graph1, GroupedGraph1)),
          Graph2 = my_digraph:create([1, 2, 3, 4], [{1, 2}, {1, 3}, {2, 4}, {3, 1}, {4, 1}]),
          GroupedGraph2 = CreateGraph([[1, 2, 3, 4]], []),
          ?assertEqual({one_for_all, [3, 4, 2, 1]},
                       optimum_supervision_tree_solver:transform(Graph2, GroupedGraph2))
       end},
      {"`GroupedVertex` with an in-degree of 0 and a length greater than or equal to 2 is monitored by `rest_for_one`.",
       fun() ->
          Graph = my_digraph:create([1, 2, 3, 4], [{1, 2}, {1, 4}, {2, 3}, {2, 4}, {3, 4}]),
          GroupedGraph = CreateGraph([[1, 2, 3], [4]], [{[1, 2, 3], [4]}]),
          ?assertEqual({rest_for_one, [4, {rest_for_one, [3, 2, 1]}]},
                       optimum_supervision_tree_solver:transform(Graph, GroupedGraph))
       end}]}.

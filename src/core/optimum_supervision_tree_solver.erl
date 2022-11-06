-module(optimum_supervision_tree_solver).

-export([group/1, sort_by_postorder/2]).

group(Graph) ->
    GroupedGraph = digraph:new(),
    Targets =
        lists:filter(fun(V) -> digraph:out_degree(Graph, V) =:= 0 end, digraph:vertices(Graph)),
    group(Graph, GroupedGraph, Targets, []).

-spec group(digraph:graph(), digraph:graph(), [digraph:vertex()], [digraph:vertex()]) ->
               digraph:graph().
group(_, GroupedGraph, [], _) -> GroupedGraph;
group(Graph, GroupedGraph, Targets, GroupedParents) ->
    SubGraph = digraph_utils:subgraph(Graph, digraph_utils:reaching(Targets, Graph)),
    Components = digraph_utils:components(SubGraph),
    GroupedTargets =
        lists:map(fun(Target) -> sort_by_postorder(Target, Graph) end,
                  lists:map(fun(Component) ->
                               lists:filter(fun(Target) -> lists:member(Target, Component) end,
                                            Targets)
                            end,
                            Components)),
    lists:foreach(fun(GroupedVertices) -> digraph:add_vertex(GroupedGraph, GroupedVertices)
                  end,
                  GroupedTargets),
    lists:foreach(fun(GroupedVertices) ->
                     lists:foreach(fun(Parent) ->
                                      % NOTE: Twist edges as we grouped up vertices
                                      case lists:any(fun(V1) ->
                                                        lists:any(fun(V2) ->
                                                                     my_digraph:has_path(Graph,
                                                                                         V1,
                                                                                         V2)
                                                                  end,
                                                                  Parent)
                                                     end,
                                                     GroupedVertices)
                                      of
                                          true ->
                                              digraph:add_edge(GroupedGraph,
                                                               GroupedVertices,
                                                               Parent);
                                          false -> ok
                                      end
                                   end,
                                   GroupedParents)
                  end,
                  GroupedTargets),
    NewTargets =
        digraph_utils:reachable(
            lists:flatten(
                lists:map(fun(V) -> digraph:in_neighbours(Graph, V) end, Targets)),
            Graph)
        -- lists:flatten(
               digraph:vertices(GroupedGraph)),
    group(Graph, GroupedGraph, NewTargets, GroupedTargets).

-spec sort_by_postorder([digraph:vertex()], digraph:graph()) -> [digraph:vertex()].
sort_by_postorder(Vertices, Graph) ->
    RevPostOrderVertices =
        lists:reverse(
            digraph_utils:postorder(Graph)),
    lists:filter(fun(V) -> lists:member(V, Vertices) end, RevPostOrderVertices).

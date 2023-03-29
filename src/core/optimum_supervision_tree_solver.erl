-module(optimum_supervision_tree_solver).

-export([solve/1, group/1, sort_by_postorder/2, transform/2]).

-type dependency_graph() :: digraph:graph().
-type dependency_graph_vertex() :: atom().
-type grouped_graph() :: digraph:graph().
-type grouped_graph_vertex() :: sets:set(dependency_graph_vertex()).

% TODO: Prevent the error from occurring in the first place in some way.
-ignore_xref(group/1).
-ignore_xref(sort_by_postorder/2).
-ignore_xref(transform/1).

-spec solve(dependency_graph:t()) -> supervision_tree:t().
solve(DependencyGraph) ->
    Graph =
        begin
            Vertices = maps:keys(DependencyGraph),
            Edges =
                [{From, To}
                 || From <- maps:keys(DependencyGraph), To <- maps:get(From, DependencyGraph)],
            my_digraph:create(Vertices, Edges)
        end,
    transform(Graph, group(Graph)).

-spec group(dependency_graph()) -> grouped_graph().
group(Graph) ->
    Pred = fun(V) -> get_strong_component(Graph, V) =:= reachable([V], Graph) end,
    Targets = sets:filter(Pred, vertices(Graph)),
    group(Graph, digraph:new(), Targets, sets:new()).

-spec group(dependency_graph(),
            grouped_graph(),
            sets:set(dependency_graph_vertex()),
            sets:set(grouped_graph_vertex())) ->
               grouped_graph().
group(Graph, GroupedGraph, Targets, PrevGroupedTargets) ->
    case sets:size(Targets) =:= 0 of
        true -> GroupedGraph;
        false ->
            SubGraph =
                digraph_utils:subgraph(Graph,
                                       digraph_utils:reaching(
                                           sets:to_list(Targets), Graph)),
            GroupedTargets =
                my_sets:map(fun(Component) -> sets:intersection(Targets, Component) end,
                            components(SubGraph)),
            % NOTE: Since `foreach` makes the code difficult to read, list comprehensions are used instead.
            _ = [digraph:add_vertex(GroupedGraph, V) || V <- sets:to_list(GroupedTargets)],
            _ = [digraph:add_edge(GroupedGraph, From, To)
                 || {From, To}
                        <- % NOTE: Twist edges as we grouped up vertices
                           lists:uniq([{GroupedVertex, PreGroupedVertex}
                                       || GroupedVertex <- sets:to_list(GroupedTargets),
                                          PreGroupedVertex <- sets:to_list(PrevGroupedTargets),
                                          V1 <- sets:to_list(GroupedVertex),
                                          V2 <- sets:to_list(PreGroupedVertex),
                                          my_digraph:has_path(Graph, V1, V2)])],
            NewTargets =
                % NOTE:
                % There may be unvisited behaviors that are not children of `Targets`
                % but on which the children of `Targets` depend.
                sets:subtract(reachable([U
                                         || V <- sets:to_list(Targets),
                                            U <- digraph:in_neighbours(Graph, V)],
                                        SubGraph),
                              Targets),
            group(Graph, GroupedGraph, NewTargets, GroupedTargets)
    end.

-spec sort_by_postorder([digraph:vertex()], digraph:graph()) -> [digraph:vertex()].
sort_by_postorder(Vertices, Graph) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:postorder(Graph)).

-spec transform(dependency_graph(), grouped_graph()) -> supervision_tree:t().
transform(Graph, GroupedGraph) ->
    case lists:filter(fun(V) -> digraph:out_degree(GroupedGraph, V) =:= 0 end,
                      digraph:vertices(GroupedGraph))
    of
        [] -> throw(impossible);
        [GroupedVertex] -> transform(Graph, GroupedGraph, GroupedVertex);
        GroupedVertices ->
            {one_for_one,
             lists:map(fun(GroupedVertex) -> transform(Graph, GroupedGraph, GroupedVertex) end,
                       GroupedVertices)}
    end.

-spec transform(dependency_graph(), grouped_graph(), grouped_graph_vertex()) ->
                   supervision_tree:t().
transform(Graph, GroupedGraph, GroupedVertex) ->
    RightChild =
        case digraph:in_neighbours(GroupedGraph, GroupedVertex) of
            [] -> nil;
            [V] -> transform(Graph, GroupedGraph, V);
            Vs -> {one_for_one, lists:map(fun(V) -> transform(Graph, GroupedGraph, V) end, Vs)}
        end,
    Strategy =
        case my_sets:any(fun(Set) -> sets:size(Set) > 0 end,
                         my_sets:map(fun(V) -> get_cyclic_strong_component(Graph, V) end,
                                     GroupedVertex))
        of
            true -> one_for_all;
            false -> rest_for_one
        end,
    case RightChild of
        nil -> {Strategy, sort_by_postorder(sets:to_list(GroupedVertex), Graph)};
        _ -> {Strategy, sort_by_postorder(sets:to_list(GroupedVertex), Graph) ++ [RightChild]}
    end.

-spec vertices(digraph:graph()) -> sets:set(digraph:vertex()).
vertices(G) ->
    sets:from_list(
        digraph:vertices(G)).

-spec get_strong_component(digraph:graph(), digraph:vertex()) ->
                              sets:set(digraph:vertex()).
get_strong_component(Digraph, Vertex) ->
    case my_digraph_utils:get_strong_component(Digraph, Vertex) of
        false -> sets:new();
        Component -> sets:from_list(Component)
    end.

-spec get_cyclic_strong_component(digraph:graph(), digraph:vertex()) ->
                                     sets:set(digraph:vertex()).
get_cyclic_strong_component(Digraph, Vertex) ->
    case my_digraph_utils:get_cyclic_strong_component(Digraph, Vertex) of
        false -> sets:new();
        Component -> sets:from_list(Component)
    end.

-spec reachable([digraph:vertex()], digraph:graph()) -> sets:set(digraph:vertex()).
reachable(Vertices, Digraph) ->
    sets:from_list(
        digraph_utils:reachable(Vertices, Digraph)).

-spec components(digraph:graph()) -> sets:set(Component)
    when Component :: sets:set(digraph:vertex()).
components(Digraph) ->
    sets:from_list([sets:from_list(C) || C <- digraph_utils:components(Digraph)]).

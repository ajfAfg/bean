-module(optimum_supervision_tree_solver).

-export([solve/1, group/1, sort_by_postorder/2, transform/2]).

-export_type([supervision_tree/0]).

-type supervision_tree() :: {supervisor:strategy(), [supervision_tree() | atom()]}.
-type dependency_graph() :: digraph:graph().
-type dependency_graph_vertex() :: atom().
-type grouped_graph() :: digraph:graph().
-type grouped_graph_vertex() :: [atom()].

% TODO: Prevent the error from occurring in the first place in some way.
-ignore_xref(group/1).
-ignore_xref(sort_by_postorder/2).
-ignore_xref(transform/1).

-spec solve(gen_server_dependencies:dependencies()) -> supervision_tree().
solve(Dependencies) ->
    Graph =
        begin
            Vertices = maps:keys(Dependencies),
            Edges =
                lists:flatten(
                    lists:map(fun(From) ->
                                 lists:map(fun(To) -> {From, To} end, maps:get(From, Dependencies))
                              end,
                              maps:keys(Dependencies))),
            my_digraph:create(Vertices, Edges)
        end,
    transform(Graph, group(Graph)).

-spec group(dependency_graph()) -> grouped_graph().
group(Graph) ->
    Pred = fun(V) -> get_strong_component(Graph, V) =:= reachable([V], Graph) end,
    Targets = [V || V <- digraph:vertices(Graph), Pred(V)],
    group(Graph, digraph:new(), Targets, []).

-spec group(dependency_graph(),
            grouped_graph(),
            [dependency_graph_vertex()],
            [grouped_graph_vertex()]) ->
               grouped_graph().
group(_, GroupedGraph, [], _) -> GroupedGraph;
group(Graph, GroupedGraph, Targets, GroupedParents) ->
    SubGraph = digraph_utils:subgraph(Graph, digraph_utils:reaching(Targets, Graph)),
    Components = components(SubGraph),
    Targets_ = sets:from_list(Targets),
    GroupedTargets =
        my_sets:map(fun(Component) -> sets:intersection(Targets_, Component) end, Components),
    my_sets:foreach(fun(GroupedVertices) ->
                       digraph:add_vertex(GroupedGraph, sets:to_list(GroupedVertices))
                    end,
                    GroupedTargets),
    GroupedParents_ = sets:from_list([sets:from_list(V) || V <- GroupedParents]),
    my_sets:foreach(fun(GroupedVertices) ->
                       my_sets:foreach(fun(Parent) ->
                                          % NOTE: Twist edges as we grouped up vertices
                                          case my_sets:any(fun(V1) ->
                                                              my_sets:any(fun(V2) ->
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
                                                                   sets:to_list(GroupedVertices),
                                                                   sets:to_list(Parent));
                                              false -> ok
                                          end
                                       end,
                                       GroupedParents_)
                    end,
                    GroupedTargets),
    NewTargets =
        % NOTE:
        % There may be unvisited behaviors that are not children of `Targets`
        % but on which the children of `Targets` depend.
        sets:subtract(reachable(lists:flatten(
                                    lists:map(fun(V) -> digraph:in_neighbours(Graph, V) end,
                                              Targets)),
                                SubGraph),
                      Targets_),
    NewTargets_ = sets:to_list(NewTargets),
    GroupedTargets_ =
        sets:to_list(
            my_sets:map(fun sets:to_list/1, GroupedTargets)),
    group(Graph, GroupedGraph, NewTargets_, GroupedTargets_).

-spec sort_by_postorder([digraph:vertex()], digraph:graph()) -> [digraph:vertex()].
sort_by_postorder(Vertices, Graph) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:postorder(Graph)).

-spec transform(dependency_graph(), grouped_graph()) -> supervision_tree().
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
                   supervision_tree().
transform(Graph, GroupedGraph, GroupedVertex) ->
    RightChild =
        case digraph:in_neighbours(GroupedGraph, GroupedVertex) of
            [] -> nil;
            [V] -> transform(Graph, GroupedGraph, V);
            Vs -> {one_for_one, lists:map(fun(V) -> transform(Graph, GroupedGraph, V) end, Vs)}
        end,
    GroupedVertex_ = sets:from_list(GroupedVertex),
    Strategy =
        case my_sets:any(fun(Set) -> sets:size(Set) > 0 end,
                         my_sets:map(fun(V) -> get_cyclic_strong_component(Graph, V) end,
                                     GroupedVertex_))
        of
            true -> one_for_all;
            false -> rest_for_one
        end,
    case RightChild of
        nil -> {Strategy, sort_by_postorder(GroupedVertex, Graph)};
        _ -> {Strategy, sort_by_postorder(GroupedVertex, Graph) ++ [RightChild]}
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

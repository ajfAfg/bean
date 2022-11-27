-module(optimum_supervision_tree_solver).

-export([solve/1, group/1, sort_by_postorder/2,
         transform_into_optimum_supervision_tree/1]).

-export_type([supervision_tree/0]).

-type supervision_tree() :: {supervisor:strategy(), [supervision_tree() | atom()]}.
-type dependency_graph() :: digraph:graph().
-type dependency_graph_vertex() :: atom().
-type grouped_graph() :: digraph:graph().
-type grouped_graph_vertex() :: [atom()].

% TODO: Prevent the error from occurring in the first place in some way.
-ignore_xref(group/1).
-ignore_xref(sort_by_postorder/2).
-ignore_xref(transform_into_optimum_supervision_tree/1).

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
    transform_into_optimum_supervision_tree(group(Graph)).

-spec group(dependency_graph()) -> grouped_graph().
group(Graph) ->
    GroupedGraph = digraph:new(),
    Targets =
        lists:filter(fun(V) ->
                        GetStrongConnectedComponent =
                            fun() ->
                               case my_digraph_utils:get_strong_component(Graph, V) of
                                   false -> [];
                                   Vertices -> Vertices
                               end
                            end,
                        lists:uniq(
                            lists:sort(GetStrongConnectedComponent()))
                        =:= lists:sort(
                                digraph_utils:reachable([V], Graph))
                     end,
                     digraph:vertices(Graph)),
    group(Graph, GroupedGraph, Targets, []).

-spec group(dependency_graph(),
            grouped_graph(),
            [dependency_graph_vertex()],
            [grouped_graph_vertex()]) ->
               grouped_graph().
group(_, GroupedGraph, [], _) -> GroupedGraph;
group(Graph, GroupedGraph, Targets, GroupedParents) ->
    SubGraph = digraph_utils:subgraph(Graph, digraph_utils:reaching(Targets, Graph)),
    Components = digraph_utils:components(SubGraph),
    GroupedTargets =
        lists:map(fun(Target) ->
                     % NOTE: Behaviors in the same group may have dependencies.
                     sort_by_postorder(Target, Graph)
                  end,
                  lists:map(fun(Component) ->
                               lists:filter(fun(Target) -> lists:member(Target, Component) end,
                                            Targets)
                            end,
                            Components)),
    lists:foreach(fun(GroupedVertices) ->
                     Label =
                         case my_digraph_utils:get_cyclic_strong_component(Graph,
                                                                           hd(GroupedVertices))
                         of
                             false -> [];
                             _ -> cyclic_strong_component
                         end,
                     digraph:add_vertex(GroupedGraph, GroupedVertices, Label)
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
        % NOTE:
        % There may be unvisited behaviors that are not children of `Targets`
        % but on which the children of `Targets` depend.
        digraph_utils:reachable(
            lists:flatten(
                lists:map(fun(V) -> digraph:in_neighbours(Graph, V) end, Targets)),
            SubGraph)
        -- Targets,
    group(Graph, GroupedGraph, NewTargets, GroupedTargets).

-spec sort_by_postorder([digraph:vertex()], digraph:graph()) -> [digraph:vertex()].
sort_by_postorder(Vertices, Graph) ->
    RevPostOrderVertices =
        lists:reverse(
            digraph_utils:postorder(Graph)),
    lists:filter(fun(V) -> lists:member(V, Vertices) end, RevPostOrderVertices).

% TODO: Not "optimal" yet.
-spec transform_into_optimum_supervision_tree(grouped_graph()) -> supervision_tree().
transform_into_optimum_supervision_tree(GroupedGraph) ->
    case lists:filter(fun(V) -> digraph:out_degree(GroupedGraph, V) =:= 0 end,
                      digraph:vertices(GroupedGraph))
    of
        [] -> throw(impossible);
        [GroupedVertex] -> transform_into_optimum_supervision_tree(GroupedGraph, GroupedVertex);
        GroupedVertices ->
            {one_for_one,
             lists:map(fun(GroupedVertex) ->
                          transform_into_optimum_supervision_tree(GroupedGraph, GroupedVertex)
                       end,
                       GroupedVertices)}
    end.

-spec transform_into_optimum_supervision_tree(grouped_graph(), grouped_graph_vertex()) ->
                                                 supervision_tree().
transform_into_optimum_supervision_tree(GroupedGraph, GroupedVertex) ->
    LeftChild =
        case digraph:in_neighbours(GroupedGraph, GroupedVertex) of
            [] -> nil;
            [V] -> transform_into_optimum_supervision_tree(GroupedGraph, V);
            Vs ->
                {one_for_one,
                 lists:map(fun(V) -> transform_into_optimum_supervision_tree(GroupedGraph, V) end,
                           Vs)}
        end,
    Strategy =
        case digraph:vertex(GroupedGraph, GroupedVertex) of
            {GroupedVertex, cyclic_strong_component} -> one_for_all;
            {GroupedVertex, []} -> rest_for_one
        end,
    case LeftChild of
        nil -> {Strategy, GroupedVertex};
        _ -> {Strategy, [LeftChild] ++ GroupedVertex}
    end.

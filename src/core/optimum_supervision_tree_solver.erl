-module(optimum_supervision_tree_solver).

-export([solve/1, take_vertex_splitters/1]).

-type dag() :: digraph:graph().
-type dag_vertex() :: [dependency_digraph:vertex()].
-type connected_dag() :: digraph:graph().
-type connected_dag_vertex() :: [dependency_digraph:vertex()].

-spec solve(dependency_graph:t()) -> supervision_tree:t().
solve(DependencyGraph) ->
    Graph = dependency_digraph:from_dependency_graph(DependencyGraph),
    transform(digraph_utils:condensation(Graph)).

% NOTE: The argument graph is assumed to be a DAG.
-spec transform(dag()) -> supervision_tree:t().
transform(DAG) ->
    case digraph_utils:components(DAG) of
        [] -> throw(impossible);
        [_] -> transform_(DAG);
        Components ->
            % NOTE: Remove an unneeded supervisor
            Fun = fun ({rest_for_one, [V]}) -> V;
                      (Other) -> Other
                  end,
            {one_for_one,
             lists:map(fun(Component) -> Fun(transform_(digraph_utils:subgraph(DAG, Component)))
                       end,
                       Components)}
    end.

-spec transform_(connected_dag()) -> supervision_tree:t().
transform_(ConnectedDAG) ->
    Candidates =
        [begin
             NextConnectedDAGs =
                 begin
                     SubGraph =
                         digraph_utils:subgraph(ConnectedDAG,
                                                digraph:vertices(ConnectedDAG) -- VertexSplitter),
                     lists:map(fun(Component) -> digraph_utils:subgraph(SubGraph, Component) end,
                               digraph_utils:components(SubGraph))
                 end,
             hd(transform__(lists:reverse(sort_by_topological_ordering(VertexSplitter,
                                                                       ConnectedDAG)),
                            NextConnectedDAGs))
         end
         || VertexSplitter <- take_vertex_splitters(ConnectedDAG)],
    hd(lists:sort(fun(Tree1, Tree2) ->
                     supervision_tree:calc_cost(Tree1) =< supervision_tree:calc_cost(Tree2)
                  end,
                  Candidates)).

-spec transform__([connected_dag_vertex()], [connected_dag()]) -> [supervision_tree:t()].
transform__([], []) -> [];
transform__([], NextConnectedDAGs) ->
    % NOTE: Remove an unneeded supervisor
    Fun = fun ({rest_for_one, [V]}) -> V;
              (Other) -> Other
          end,
    [{one_for_one, lists:map(Fun, lists:map(fun transform_/1, NextConnectedDAGs))}];
transform__([C | _] = Components, NextDAGs) when length(C) =:= 1 ->
    {Cs1, Cs2} = lists:splitwith(fun(C_) -> length(C_) =:= 1 end, Components),
    [{rest_for_one, lists:flatten(Cs1) ++ transform__(Cs2, NextDAGs)}];
transform__([C | Components], NextDAGs) ->
    [{one_for_all, C ++ transform__(Components, NextDAGs)}].

% NOTE: The argument graph is assumed to be a DAG.
-spec sort_by_topological_ordering([dag_vertex()], dag()) -> [dag_vertex()].
sort_by_topological_ordering(Vertices, DAG) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:topsort(DAG)).

-spec take_vertex_splitters(connected_dag()) -> [[connected_dag_vertex()]].
take_vertex_splitters(ConnectedDAG) ->
    lists:usort([sets:to_list(
                     lists:foldl(fun(Vs, Acc) -> sets:intersection(Acc, sets:from_list(Vs)) end,
                                 sets:from_list(
                                     digraph:vertices(ConnectedDAG)),
                                 [digraph_utils:reachable([U], ConnectedDAG)
                                  || U <- digraph:vertices(ConnectedDAG),
                                     digraph:in_degree(ConnectedDAG, U) =:= 0,
                                     my_digraph:has_path(ConnectedDAG, U, V)]))
                 || V <- digraph:vertices(ConnectedDAG),
                    digraph:out_degree(ConnectedDAG, V) =:= 0]).

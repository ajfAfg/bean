-module(optimum_supervision_tree_solver).

-export([solve/2]).

-type dag() :: digraph:graph().
-type dag_vertex() :: [dependency_digraph:vertex()].
-type connected_dag() :: digraph:graph().
-type connected_dag_vertex() :: [dependency_digraph:vertex()].

-spec solve(dependency_graph:t(),
            all_local_minimum_vertex_splitters_solver:take_all_local_minimum_vertex_splitters()) ->
               supervision_tree:t().
solve(DependencyGraph, TakeAllLocalMinimumVertexSplitters) ->
    Graph = dependency_digraph:from_dependency_graph(DependencyGraph),
    transform(digraph_utils:condensation(Graph), TakeAllLocalMinimumVertexSplitters).

% NOTE: The argument graph is assumed to be a DAG.
-spec transform(dag(),
                all_local_minimum_vertex_splitters_solver:take_all_local_minimum_vertex_splitters()) ->
                   supervision_tree:t().
transform(DAG, TakeAllLocalMinimumVertexSplitters) ->
    case digraph_utils:components(DAG) of
        [] -> throw(impossible);
        [_] -> transform_(DAG, TakeAllLocalMinimumVertexSplitters);
        Components ->
            % NOTE: Remove an unneeded supervisor
            Fun = fun ({rest_for_one, [V]}) -> V;
                      (Other) -> Other
                  end,
            {one_for_one,
             lists:map(fun(Component) ->
                          Fun(transform_(digraph_utils:subgraph(DAG, Component),
                                         TakeAllLocalMinimumVertexSplitters))
                       end,
                       Components)}
    end.

-spec transform_(connected_dag(),
                 all_local_minimum_vertex_splitters_solver:take_all_local_minimum_vertex_splitters()) ->
                    supervision_tree:t().
transform_(ConnectedDAG, TakeAllLocalMinimumVertexSplitters) ->
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
                            NextConnectedDAGs,
                            TakeAllLocalMinimumVertexSplitters))
         end
         || VertexSplitter <- TakeAllLocalMinimumVertexSplitters(ConnectedDAG)],
    hd(lists:sort(fun(Tree1, Tree2) ->
                     supervision_tree:calc_cost(Tree1) =< supervision_tree:calc_cost(Tree2)
                  end,
                  Candidates)).

-spec transform__([connected_dag_vertex()],
                  [connected_dag()],
                  all_local_minimum_vertex_splitters_solver:take_all_local_minimum_vertex_splitters()) ->
                     [supervision_tree:t()].
transform__([], [], _TakeAllLocalMinimumVertexSplitters) -> [];
transform__([], NextConnectedDAGs, TakeAllLocalMinimumVertexSplitters) ->
    % NOTE: Remove an unneeded supervisor
    Fun = fun ({rest_for_one, [V]}) -> V;
              (Other) -> Other
          end,
    [{one_for_one,
      lists:map(Fun,
                lists:map(fun(G) -> transform_(G, TakeAllLocalMinimumVertexSplitters) end,
                          NextConnectedDAGs))}];
transform__([C | _] = Components, NextDAGs, TakeAllLocalMinimumVertexSplitters)
    when length(C) =:= 1 ->
    {Cs1, Cs2} = lists:splitwith(fun(C_) -> length(C_) =:= 1 end, Components),
    [{rest_for_one,
      lists:flatten(Cs1) ++ transform__(Cs2, NextDAGs, TakeAllLocalMinimumVertexSplitters)}];
transform__([C | Components], NextDAGs, TakeAllLocalMinimumVertexSplitters) ->
    [{one_for_all,
      C ++ transform__(Components, NextDAGs, TakeAllLocalMinimumVertexSplitters)}].

% NOTE: The argument graph is assumed to be a DAG.
-spec sort_by_topological_ordering([dag_vertex()], dag()) -> [dag_vertex()].
sort_by_topological_ordering(Vertices, DAG) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:topsort(DAG)).

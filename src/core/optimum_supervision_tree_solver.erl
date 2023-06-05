-module(optimum_supervision_tree_solver).

-export([solve/1, take_split_vertices/1]).

-type dag() :: digraph:graph().
-type dag_vertex() :: [dependency_digraph:vertex()].

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
             lists:map(fun(Component) ->
                          Fun(transform_(digraph_utils:subgraph(
                                             my_digraph_utils:clone(DAG), Component)))
                       end,
                       Components)}
    end.

-spec transform_(dag()) -> supervision_tree:t().
transform_(DAG) ->
    SplitVertices = take_split_vertices(DAG),
    case length(SplitVertices) =:= length(digraph:vertices(DAG)) of
        true ->
            case digraph:vertices(DAG) of
                [] -> throw(impossible);
                Vs -> hd(transform__(lists:reverse(sort_by_topological_ordering(Vs, DAG)), []))
            end;
        false ->
            NextDAGs =
                begin
                    SubGraph =
                        digraph_utils:subgraph(
                            my_digraph_utils:clone(DAG), digraph:vertices(DAG) -- SplitVertices),
                    lists:map(fun(Component) ->
                                 digraph_utils:subgraph(
                                     my_digraph_utils:clone(SubGraph), Component)
                              end,
                              digraph_utils:components(SubGraph))
                end,
            hd(transform__(lists:reverse(sort_by_topological_ordering(SplitVertices, DAG)),
                           NextDAGs))
    end.

-spec transform__([dag_vertex()], [dag()]) -> [supervision_tree:t()].
transform__([], []) -> [];
transform__([], NextDAGs) ->
    % NOTE: Remove an unneeded supervisor
    Fun = fun ({rest_for_one, [V]}) -> V;
              (Other) -> Other
          end,
    [{one_for_one, lists:map(Fun, lists:map(fun transform_/1, NextDAGs))}];
transform__([C | _] = Components, NextDAGs) when length(C) =:= 1 ->
    {Cs1, Cs2} = lists:splitwith(fun(C_) -> length(C_) =:= 1 end, Components),
    [{rest_for_one, lists:flatten(Cs1) ++ transform__(Cs2, NextDAGs)}];
transform__([C | Components], NextDAGs) ->
    [{one_for_all, C ++ transform__(Components, NextDAGs)}].

% NOTE: The argument graph is assumed to be a DAG.
-spec sort_by_topological_ordering([digraph:vertex()], digraph:graph()) ->
                                      [digraph:vertex()].
sort_by_topological_ordering(Vertices, DAG) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:topsort(DAG)).

% NOTE:
% Split vertices (like cut vertex) are a minimal set of vertices that,
% when removed from a graph G, divide it into two or more connected graphs,
% where for all the connected graphs g₁ and g₂, exists v₁ ∈ g₁, v₂ ∈ g₂,
% v₁ is not reachable by v₂ in G.
% Note that the argument graph is assumed to be a connected DAG and a connected graph.
-spec take_split_vertices(digraph:graph()) -> [digraph:vertex()].
take_split_vertices(ConnectedDAG) ->
    sets:to_list(
        lists:foldl(fun(Vs, Acc) -> sets:intersection(Acc, sets:from_list(Vs)) end,
                    sets:from_list(
                        digraph:vertices(ConnectedDAG)),
                    lists:map(fun(V) -> digraph_utils:reachable([V], ConnectedDAG) end,
                              lists:filter(fun(V) -> digraph:in_degree(ConnectedDAG, V) =:= 0 end,
                                           digraph:vertices(ConnectedDAG))))).

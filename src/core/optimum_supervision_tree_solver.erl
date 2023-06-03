-module(optimum_supervision_tree_solver).

-export([solve/1, search_split_vertex/1]).

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
    case search_split_vertex(DAG) of
        false ->
            case digraph:vertices(DAG) of
                [] -> throw(impossible);
                Vs -> hd(transform__(lists:reverse(sort_by_topological_ordering(Vs, DAG)), []))
            end;
        {value, V} ->
            Reachable = digraph_utils:reachable([V], DAG),
            NextDAGs =
                begin
                    SubGraph =
                        digraph_utils:subgraph(
                            my_digraph_utils:clone(DAG), digraph:vertices(DAG) -- Reachable),
                    lists:map(fun(Component) ->
                                 digraph_utils:subgraph(
                                     my_digraph_utils:clone(SubGraph), Component)
                              end,
                              digraph_utils:components(SubGraph))
                end,
            hd(transform__(lists:reverse(sort_by_topological_ordering(Reachable, DAG)), NextDAGs))
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
% A split vertex (like cut vertex) is defined as
% a vertex reaching every vertex of a set of vertices that,
% when removed from a graph, become two or more connected graphs.
% This function searches the argument graph for topologically sorted vertices in reverse order
% and returns the split vertex found.
% Note that the argument graph is assumed to be a DAG and a connected graph.
-spec search_split_vertex(digraph:graph()) -> {value, digraph:vertex()} | false.
search_split_vertex(ConnectedDAG) ->
    Vertices =
        lists:reverse(
            digraph_utils:topsort(ConnectedDAG)),
    Pred =
        fun(V) ->
           NewGraph = my_digraph_utils:clone(ConnectedDAG),
           digraph:del_vertices(NewGraph, digraph_utils:reachable([V], NewGraph)),
           length(digraph_utils:components(NewGraph)) >= 2
        end,
    lists:search(Pred, Vertices).

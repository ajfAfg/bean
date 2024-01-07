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
             % NOTE: `VertexSplitter` =/= []
             SubAcacia =
                 option:get(transform_into_acacia(lists:reverse(sort_by_topological_ordering(VertexSplitter,
                                                                                             ConnectedDAG)))),
             SubTrees =
                 begin
                     SubGraph =
                         digraph_utils:subgraph(ConnectedDAG,
                                                digraph:vertices(ConnectedDAG) -- VertexSplitter),
                     SubConnectedDAGs =
                         lists:map(fun(Component) -> digraph_utils:subgraph(SubGraph, Component)
                                   end,
                                   digraph_utils:components(SubGraph)),
                     % NOTE: Remove an unneeded supervisor
                     Fun = fun ({rest_for_one, [V]}) -> V;
                               (Other) -> Other
                           end,
                     [Fun(transform_(SubConnectedDAG, TakeAllLocalMinimumVertexSplitters))
                      || SubConnectedDAG <- SubConnectedDAGs]
                 end,
             case SubTrees of
                 [] -> SubAcacia;
                 _ -> merge(SubAcacia, {one_for_one, SubTrees})
             end
         end
         || VertexSplitter <- TakeAllLocalMinimumVertexSplitters(ConnectedDAG)],
    hd(lists:sort(fun(Tree1, Tree2) ->
                     supervision_tree:calc_cost(Tree1) =< supervision_tree:calc_cost(Tree2)
                  end,
                  Candidates)).

% NOTE: The argument graph is assumed to be a DAG.
-spec sort_by_topological_ordering([dag_vertex()], dag()) -> [dag_vertex()].
sort_by_topological_ordering(Vertices, DAG) ->
    lists:filter(fun(V) -> lists:member(V, Vertices) end, digraph_utils:topsort(DAG)).

% NOTE:
% Assume the argument is a vertex splitter and
% is sorted in reverse order of topological sorting.
-spec transform_into_acacia([connected_dag_vertex()]) -> option:t(supervision_tree:t()).
transform_into_acacia([]) -> none;
transform_into_acacia([V | _] = VertexSplitter) when length(V) =:= 1 ->
    {Vs1, Vs2} = lists:splitwith(fun(C_) -> length(C_) =:= 1 end, VertexSplitter),
    Children =
        case transform_into_acacia(Vs2) of
            {some, Tree} -> lists:flatten(Vs1) ++ [Tree];
            none -> lists:flatten(Vs1)
        end,
    {some, {rest_for_one, Children}};
transform_into_acacia([V | Vs]) ->
    Children =
        case transform_into_acacia(Vs) of
            {some, Tree} -> V ++ [Tree];
            none -> V
        end,
    {some, {one_for_all, Children}}.

% NOTE: Assume the arguments are standard trees.
-spec merge(supervision_tree:t(), supervision_tree:t()) -> supervision_tree:t().
merge({Strategy, Children}, Tree) ->
    LastChild = lists:last(Children),
    case supervision_tree:is_tree(LastChild) of
        true -> {Strategy, lists:droplast(Children) ++ [merge(LastChild, Tree)]};
        false -> {Strategy, Children ++ [Tree]}
    end.

-module(all_local_minimum_vertex_splitters_solver).

-export([solve_in_polynomial_time_without_correctness/1,
         solve_in_exp_time_with_correctness/1]).

-export_type([take_all_local_minimum_vertex_splitters/0]).

% NOTE: The argument graph is assumed to be a connected DAG.
-type take_all_local_minimum_vertex_splitters() ::
    fun((dependency_digraph:t()) -> [[dependency_digraph:vertex()]]).

%% ===================================================================
%% Public API
%% ===================================================================
% NOTE:
% The argument graph is assumed to be a connected DAG.
% To reduce computation time, do not check whether `ConnectedDAG` is a connected DAG.
-spec solve_in_polynomial_time_without_correctness(dependency_digraph:t()) ->
                                                      [[dependency_digraph:vertex()]].
solve_in_polynomial_time_without_correctness(ConnectedDAG) ->
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

% NOTE:
% The argument graph is assumed to be a connected DAG.
% To reduce computation time, do not check whether `ConnectedDAG` is a connected DAG.
-spec solve_in_exp_time_with_correctness(dependency_digraph:t()) ->
                                            [[dependency_digraph:vertex()]].
solve_in_exp_time_with_correctness(ConnectedDAG) ->
    Candidates =
        [Vertices
         || Vertices
                <- my_lists:power(
                       digraph:vertices(ConnectedDAG)),
            dependency_digraph:is_vertex_splitter(ConnectedDAG, Vertices)],
    Candidates2 =
        begin
            MinimumVertexSplittersForEveryExitVertex =
                lists:foldl(fun(VertexSplitter, Acc) ->
                               lists:foldl(fun(ExitVertex, Acc2) ->
                                              maps:update_with(ExitVertex,
                                                               fun(VertexSplitters) ->
                                                                  case
                                                                      compare(length(VertexSplitter),
                                                                              length(hd(VertexSplitters)))
                                                                  of
                                                                      less -> [VertexSplitter];
                                                                      greater -> VertexSplitters;
                                                                      equal ->
                                                                          [VertexSplitter
                                                                           | VertexSplitters]
                                                                  end
                                                               end,
                                                               Acc2)
                                           end,
                                           Acc,
                                           [V
                                            || V <- VertexSplitter,
                                               is_exit_vertex(ConnectedDAG, V)])
                            end,
                            maps:from_keys(exit_vertices(ConnectedDAG),
                                           [digraph:vertices(ConnectedDAG)]),
                            Candidates),
            lists:usort(
                lists:map(fun lists:sort/1,
                          my_lists:flatten(
                              maps:values(MinimumVertexSplittersForEveryExitVertex))))
        end,
    [S1
     || S1 <- Candidates2,
        not
            lists:any(fun(S2) ->
                         sets:is_subset(
                             sets:from_list(S2), sets:from_list(S1))
                      end,
                      lists:delete(S1, Candidates2))].

%% ===================================================================
%% Private API
%% ===================================================================
-spec compare(term(), term()) -> less | greater | equal.
compare(X, Y) when X < Y -> less;
compare(X, Y) when X > Y -> greater;
compare(_, _) -> equal.

-spec exit_vertices(digraph:graph()) -> [digraph:vertex()].
exit_vertices(Digraph) ->
    [V || V <- digraph:vertices(Digraph), is_exit_vertex(Digraph, V)].

-spec is_exit_vertex(digraph:graph(), digraph:vertex()) -> boolean().
is_exit_vertex(Digraph, Vertex) -> digraph:out_degree(Digraph, Vertex) =:= 0.

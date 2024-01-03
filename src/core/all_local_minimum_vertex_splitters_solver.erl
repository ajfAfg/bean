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
    lists:filter(fun(Candidate) ->
                    not
                        lists:any(fun(Vertices) ->
                                     dependency_digraph:is_vertex_splitter(ConnectedDAG,
                                                                           Candidate -- Vertices)
                                  end,
                                  lists:delete([], my_lists:power(Candidate)))
                 end,
                 Candidates).

%% ===================================================================
%% Private API
%% ===================================================================

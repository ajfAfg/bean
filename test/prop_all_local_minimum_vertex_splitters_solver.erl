-module(prop_all_local_minimum_vertex_splitters_solver).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

dependency_graph_to_connected_dag(G) ->
    Digraph =
        digraph_utils:condensation(
            dependency_digraph:from_dependency_graph(G)),
    digraph_utils:subgraph(Digraph, hd(digraph_utils:components(Digraph))).

dependency_connected_dag(MaxVertexNum) ->
    {'$call',
     ?MODULE,
     dependency_graph_to_connected_dag,
     [dependency_graph_generator:dependency_graph(MaxVertexNum)]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_solve_in_polynomial_time_without_correctness1(doc) ->
    "Satisfy constraint 1 of the vertex splitter".

prop_solve_in_polynomial_time_without_correctness1() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(1000),
            begin
                lists:all(fun(X) -> X end,
                          [dependency_digraph:satisfy_vertex_splitter_constraint1(ConnectedDAG,
                                                                                  VertexSplitter)
                           || VertexSplitter
                                  <- all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness(ConnectedDAG)])
            end).

% TODO:
% The current implementation of `solve_in_polynomial_time_without_correctness`
% does not always satisfy the vertex splitter constraint 2, "All" and "Local minimum".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_solve_in_exp_time_with_correctness1(doc) ->
    "Satisfy constraint 1 of the vertex splitter".

prop_solve_in_exp_time_with_correctness1() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(20),
            begin
                lists:all(fun(X) -> X end,
                          [dependency_digraph:satisfy_vertex_splitter_constraint1(ConnectedDAG,
                                                                                  VertexSplitter)
                           || VertexSplitter
                                  <- all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(ConnectedDAG)])
            end).

prop_solve_in_exp_time_with_correctness2(doc) ->
    "Satisfy constraint 2 of the vertex splitter".

prop_solve_in_exp_time_with_correctness2() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(20),
            begin
                lists:all(fun(X) -> X end,
                          [dependency_digraph:satisfy_vertex_splitter_constraint2(ConnectedDAG,
                                                                                  VertexSplitter)
                           || VertexSplitter
                                  <- all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(ConnectedDAG)])
            end).

prop_solve_in_exp_time_with_correctness3(doc) ->
    "The element of the return value is a minimal vertex splitter".

prop_solve_in_exp_time_with_correctness3() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(20),
            begin
                lists:all(fun(VertexSplitter) ->
                             lists:all(fun(Vertex) ->
                                          Vertices = lists:delete(Vertex, VertexSplitter),
                                          not
                                              dependency_digraph:is_vertex_splitter(ConnectedDAG,
                                                                                    Vertices)
                                       end,
                                       VertexSplitter)
                          end,
                          all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(ConnectedDAG))
            end).

prop_solve_in_exp_time_with_correctness4(doc) ->
    "The return value equals the vertices of the argument graph if only one entrance vertex".

prop_solve_in_exp_time_with_correctness4() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(20),
            begin
                ?IMPLIES(length([V
                                 || V <- digraph:vertices(ConnectedDAG),
                                    digraph:in_degree(ConnectedDAG, V) =:= 0])
                         =:= 1,
                         lists:sort(
                             digraph:vertices(ConnectedDAG))
                         =:= lists:sort(hd(all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(ConnectedDAG))))
            end).

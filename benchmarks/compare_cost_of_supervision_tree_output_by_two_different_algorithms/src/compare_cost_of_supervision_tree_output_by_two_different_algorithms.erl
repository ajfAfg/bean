-module(compare_cost_of_supervision_tree_output_by_two_different_algorithms).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    io:format("vertex_num(no), edge_num(no), avg_cost_of_polynomial_time_without_correctness(no), avg_cost_of_exp_time_with_correctness(no)~n"),

    lists:foreach(fun({VertexNum, EdgeNum}) ->
                     Cost1AndCost2ZipList =
                         lists:map(fun(_) ->
                                      Graph =
                                          dependency_graph_generator:generate_randomly(VertexNum,
                                                                                       EdgeNum),
                                      Cost1 =
                                          supervision_tree:calc_cost(
                                              optimum_supervision_tree_solver:solve(Graph,
                                                                                    fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1)),
                                      Cost2 =
                                          supervision_tree:calc_cost(
                                              optimum_supervision_tree_solver:solve(Graph,
                                                                                    fun all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness/1)),
                                      {Cost1, Cost2}
                                   end,
                                   lists:seq(1, parameters:trial_num())),

                     AvgCost1 =
                         average(lists:map(fun(X) -> element(1, X) end, Cost1AndCost2ZipList)),
                     AvgCost2 =
                         average(lists:map(fun(X) -> element(2, X) end, Cost1AndCost2ZipList)),
                     io:format("~p, ~p, ~p, ~p~n", [VertexNum, EdgeNum, AvgCost1, AvgCost2])
                  end,
                  parameters:vertex_num_and_edge_num_list()),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
-spec average([number()]) -> number().
average(NumberList) -> lists:sum(NumberList) / length(NumberList).

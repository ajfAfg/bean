-module(measure_execution_time_of_function_that_solve_optimal_supervision_tree).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    io:format("vertex_num(no), edge_num(no), execution_time(s)~n"),

    lists:foreach(fun({VertexNum, EdgeNum}) ->
                     lists:foreach(fun(_) ->
                                      Graph =
                                          dependency_graph_generator:generate_randomly(VertexNum,
                                                                                       EdgeNum),

                                      StartTime = erlang:monotonic_time(),
                                      _ = optimum_supervision_tree_solver:solve(Graph,
                                                                                fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1),
                                      StopTime = erlang:monotonic_time(),

                                      ExecutionTimeStr =
                                          begin
                                              ExecutionTime = StopTime - StartTime,
                                              Second =
                                                  erlang:convert_time_unit(ExecutionTime,
                                                                           native,
                                                                           second),
                                              Millisecond =
                                                  erlang:convert_time_unit(ExecutionTime,
                                                                           native,
                                                                           millisecond),
                                              io_lib:format("~B.~3..0B~n",
                                                            [Second,
                                                             Millisecond
                                                             - erlang:convert_time_unit(Second,
                                                                                        second,
                                                                                        millisecond)])
                                          end,
                                      io:format("~p,~p,~s", [VertexNum, EdgeNum, ExecutionTimeStr])
                                   end,
                                   lists:seq(1, parameters:trial_num()))
                  end,
                  parameters:vertex_num_and_edge_num_list()),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

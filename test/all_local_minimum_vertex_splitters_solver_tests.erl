-module(all_local_minimum_vertex_splitters_solver_tests).

-include_lib("eunit/include/eunit.hrl").

solve_in_polynomial_time_without_correctness_test_() ->
    {inparallel,
     [{"Critical test cases",
       fun() ->
          G1 = my_digraph:create(
                   lists:seq(1, 6), [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[4, 6]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness(G1)))),
          G2 = my_digraph:create(
                   lists:seq(1, 7),
                   [{1, 3}, {1, 5}, {2, 3}, {2, 4}, {2, 7}, {4, 3}, {4, 5}, {4, 7}, {5, 6}]),
          % NOTE: `[2, 3, 4, 5, 6, 7]` is not a vertex splitter (Counter-example of correctness)
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[3, 5, 6], [2, 3, 4, 5, 6, 7]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness(G2)))),
          G3 = my_digraph:create(
                   lists:seq(1, 3), [{1, 2}, {2, 3}]),
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[1, 2, 3]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness(G3))))
       end}]}.

solve_in_exp_time_with_correctness_test_() ->
    {inparallel,
     [{"Critical test cases",
       fun() ->
          G1 = my_digraph:create(
                   lists:seq(1, 6), [{1, 3}, {1, 4}, {2, 4}, {2, 5}, {3, 6}, {4, 6}, {5, 6}]),
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[4, 6]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(G1)))),
          G2 = my_digraph:create(
                   lists:seq(1, 7),
                   [{1, 3}, {1, 5}, {2, 3}, {2, 4}, {2, 7}, {4, 3}, {4, 5}, {4, 7}, {5, 6}]),
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[3, 5, 6]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(G2)))),
          G3 = my_digraph:create(
                   lists:seq(1, 3), [{1, 2}, {2, 3}]),
          ?assertEqual(lists:sort(
                           lists:map(fun lists:sort/1, [[1, 2, 3]])),
                       lists:sort(
                           lists:map(fun lists:sort/1,
                                     all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness(G3))))
       end}]}.

-module(supervision_tree_tests).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [sort/1]).

from_supervisor_specs_test_() ->
    TestCases =
        [[{s1, {#{strategy => one_for_one}, [#{id => g1}, #{id => g2}]}}],
         [{s1, {#{strategy => one_for_one}, [#{id => g1}, #{id => g2}, #{id => s2}]}},
          {s2, {#{strategy => rest_for_one}, [#{id => g3}]}}]],
    {inparallel,
     [{"The Return value is a tree.",
       fun() ->
          [Sups1, Sups2] = TestCases,
          ?assert(digraph_utils:is_tree(
                      supervision_tree:from_supervisor_specs(Sups1))),
          ?assert(digraph_utils:is_tree(
                      supervision_tree:from_supervisor_specs(Sups2)))
       end},
      {"Retains all vertices.",
       fun() ->
          [Sups1, Sups2] = TestCases,
          ?assertEqual(sort([s1, g1, g2]),
                       sort(digraph:vertices(
                                supervision_tree:from_supervisor_specs(Sups1)))),
          ?assertEqual(sort([s1, s2, g1, g2, g3]),
                       sort(digraph:vertices(
                                supervision_tree:from_supervisor_specs(Sups2))))
       end},
      {"Retains all edges.",
       fun() ->
          [Sups1, Sups2] = TestCases,
          ?assertEqual(sort([g1, g2]),
                       sort(digraph:out_neighbours(
                                supervision_tree:from_supervisor_specs(Sups1), s1))),

          ?assertEqual(sort([g1, g2, s2]),
                       sort(digraph:out_neighbours(
                                supervision_tree:from_supervisor_specs(Sups2), s1))),
          ?assertEqual(sort([g3]),
                       sort(digraph:out_neighbours(
                                supervision_tree:from_supervisor_specs(Sups2), s2)))
       end},
      {"Retains all restart strategies.",
       fun() ->
          [Sups1, Sups2] = TestCases,
          ?assertMatch({s1, #{strategy := one_for_one}},
                       digraph:vertex(
                           supervision_tree:from_supervisor_specs(Sups1), s1)),

          ?assertMatch({s1, #{strategy := one_for_one}},
                       digraph:vertex(
                           supervision_tree:from_supervisor_specs(Sups2), s1)),
          ?assertMatch({s2, #{strategy := rest_for_one}},
                       digraph:vertex(
                           supervision_tree:from_supervisor_specs(Sups2), s2))
       end},
      {"Retains all children orders.",
       fun() ->
          [Sups1, Sups2] = TestCases,
          {s1, #{children_order := Order1_1}} =
              digraph:vertex(
                  supervision_tree:from_supervisor_specs(Sups1), s1),
          ?assertEqual(1, Order1_1(g1)),
          ?assertEqual(2, Order1_1(g2)),

          {s1, #{children_order := Order2_1}} =
              digraph:vertex(
                  supervision_tree:from_supervisor_specs(Sups2), s1),
          ?assertEqual(1, Order2_1(g1)),
          ?assertEqual(2, Order2_1(g2)),
          ?assertEqual(3, Order2_1(s2)),

          {s2, #{children_order := Order2_2}} =
              digraph:vertex(
                  supervision_tree:from_supervisor_specs(Sups2), s2),
          ?assertEqual(1, Order2_2(g3))
       end}]}.

calc_cost_test_() ->
    {inparallel,
     [{"If all restart strategies are `one_for_one`, the number of gen_servers and the cost is the same.",
       fun() ->
          Sups1 = [{s1, {#{strategy => one_for_one}, [#{id => g1}, #{id => g2}]}}],
          ?assertEqual(2,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups1))),

          Sups2 =
              [{s1, {#{strategy => one_for_one}, [#{id => g1}, #{id => s1}]}},
               {s2, {#{strategy => one_for_one}, [#{id => g2}, #{id => g3}, #{id => g4}]}}],
          ?assertEqual(4,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups2)))
       end},
      {"If a restart strategy is `one_for_all`, the cost of siblings is also added.",
       fun() ->
          Sups1 = [{s1, {#{strategy => one_for_all}, [#{id => g1}, #{id => g2}]}}],
          ?assertEqual(2 + 2,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups1))),

          Sups2 =
              [{s1, {#{strategy => one_for_all}, [#{id => g1}, #{id => s2}]}},
               {s2, {#{strategy => one_for_all}, [#{id => g2}, #{id => g3}, #{id => g4}]}}],
          ?assertEqual(5 + 3 + 3 + 3,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups2)))
       end},
      {"If a restart strategy is `rest_for_one`, the cost of REST siblings is also added.",
       fun() ->
          Sups1 = [{s1, {#{strategy => rest_for_one}, [#{id => g1}, #{id => g2}]}}],
          ?assertEqual(2 + 1,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups1))),

          Sups2 =
              [{s1, {#{strategy => rest_for_one}, [#{id => g1}, #{id => s2}]}},
               {s2, {#{strategy => rest_for_one}, [#{id => g2}, #{id => g3}, #{id => g4}]}}],
          ?assertEqual(5 + 3 + 2 + 1,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups2)))
       end},
      {"Restart strategies can be mixed",
       fun() ->
          Sups =
              [{s1, {#{strategy => one_for_one}, [#{id => s2}, #{id => s3}]}},
               {s2, {#{strategy => rest_for_one}, [#{id => g5}, #{id => s4}]}},
               {s3, {#{strategy => rest_for_one}, [#{id => g6}, #{id => g7}]}},
               {s4,
                {#{strategy => one_for_all},
                 [#{id => g1}, #{id => g2}, #{id => g3}, #{id => g4}]}}],
          ?assertEqual(4 + 4 + 4 + 4 + 6 + 2 + 1,
                       supervision_tree:calc_cost(
                           supervision_tree:from_supervisor_specs(Sups)))
       end}]}.

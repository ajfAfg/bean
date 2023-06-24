-module(supervision_tree_tests).

-include_lib("eunit/include/eunit.hrl").

are_isomorphic_test_() ->
    {inparallel,
     [{"If they are equal, then they are isomorphic",
       fun() ->
          Trees =
              [{one_for_one, []},
               {one_for_all, []},
               {rest_for_one, []},
               {one_for_one, [1]},
               {one_for_all, [1]},
               {rest_for_one, [1]}],
          lists:foreach(fun(T) -> ?assertEqual(T =:= T, supervision_tree:are_isomorphic(T, T)) end,
                        Trees)
       end},
      {"If the strategy is `one_for_one` or `one_for_all`, then the order of the children is not taken into account for isomorphism",
       fun() ->
          ?assert(supervision_tree:are_isomorphic({one_for_one, [1, 2]}, {one_for_one, [2, 1]})),
          ?assert(supervision_tree:are_isomorphic({one_for_all, [1, 2]}, {one_for_all, [2, 1]})),
          ?assert(supervision_tree:are_isomorphic({one_for_one,
                                                   [{one_for_one, [1]}, {one_for_all, [2]}]},
                                                  {one_for_one,
                                                   [{one_for_all, [2]}, {one_for_one, [1]}]})),
          ?assert(supervision_tree:are_isomorphic({one_for_all,
                                                   [{one_for_one, [1]}, {one_for_all, [2]}]},
                                                  {one_for_all,
                                                   [{one_for_all, [2]}, {one_for_one, [1]}]}))
       end},
      {"If the strategy is `rest_for_one`, then the order of the children is taken into account for isomorphism",
       fun() ->
          ?assert(supervision_tree:are_isomorphic({rest_for_one, [1, 2]}, {rest_for_one, [1, 2]})),
          ?assertNot(supervision_tree:are_isomorphic({rest_for_one, [1, 2]},
                                                     {rest_for_one, [2, 1]}))
       end}]}.

calc_cost_test_() ->
    {inparallel,
     [{"one_for_all",
       fun() ->
          ?assertEqual(16, supervision_tree:calc_cost({one_for_all, [a, b, c, d]})),
          ?assertEqual(27,
                       supervision_tree:calc_cost({one_for_all,
                                                   [a,
                                                    {one_for_all, [b, c]},
                                                    {one_for_all, [d, e, f]},
                                                    g]}))
       end},
      {"one_for_one",
       fun() ->
          ?assertEqual(4, supervision_tree:calc_cost({one_for_one, [a, b, c, d]})),
          ?assertEqual(7,
                       supervision_tree:calc_cost({one_for_one,
                                                   [a,
                                                    {one_for_one, [b, c]},
                                                    {one_for_one, [d, e, f]},
                                                    g]}))
       end},
      {"rest_for_one",
       fun() ->
          ?assertEqual(10, supervision_tree:calc_cost({rest_for_one, [a, b, c, d]})),
          ?assertEqual(17,
                       supervision_tree:calc_cost({rest_for_one,
                                                   [a,
                                                    {rest_for_one, [b, c]},
                                                    {rest_for_one, [d, e, f]},
                                                    g]}))
       end}]}.

-module(my_digraph_utils_tests).

-include_lib("eunit/include/eunit.hrl").

get_strong_component_test_() ->
    {inparallel,
     [{"Can get a singleton",
       ?_assertEqual([1],
                     my_digraph_utils:get_strong_component(
                         my_digraph:create([1], []), 1))},
      {"Can get a cycle",
       ?_assertEqual([1, 2, 3],
                     lists:sort(
                         my_digraph_utils:get_strong_component(
                             my_digraph:create([1, 2, 3], [{1, 2}, {2, 3}, {3, 1}]), 1)))},
      {"Can get a strong connected component",
       ?_assertEqual([1, 2, 3, 4],
                     lists:sort(
                         my_digraph_utils:get_strong_component(
                             my_digraph:create(
                                 lists:seq(1, 4), [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}]),
                             1)))}]}.

get_cyclic_strong_component_test_() ->
    {inparallel,
     [{"Cannot get a singleton",
       ?_assertEqual(false,
                     my_digraph_utils:get_cyclic_strong_component(
                         my_digraph:create([1], []), 1))},
      {"Can get a cycle",
       ?_assertEqual([1, 2, 3],
                     lists:sort(
                         my_digraph_utils:get_cyclic_strong_component(
                             my_digraph:create([1, 2, 3], [{1, 2}, {2, 3}, {3, 1}]), 1)))},
      {"Can get a strong connected component",
       ?_assertEqual([1, 2, 3, 4],
                     lists:sort(
                         my_digraph_utils:get_cyclic_strong_component(
                             my_digraph:create(
                                 lists:seq(1, 4), [{1, 2}, {2, 3}, {2, 4}, {3, 1}, {4, 1}]),
                             1)))}]}.

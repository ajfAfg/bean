-module(gen_server_dependencies_tests).

-include_lib("eunit/include/eunit.hrl").

extract_dependencies_test_() ->
    {inparallel,
     [{"extract `gen_server` dependencies.",
       ?_assertEqual(#{first_server => [second_server],
                       second_server => [third_server],
                       third_server => []},
                     gen_server_dependencies:extract_dependencies([c_modules:first_server(),
                                                                   c_modules:second_server(),
                                                                   c_modules:third_server()]))},
      {"ignore modules other than `gen_server`",
       ?_assertEqual(#{fib_server => []},
                     gen_server_dependencies:extract_dependencies([c_modules:fib_server(),
                                                                   c_modules:foo()]))}]}.

group_test_() ->
    {inparallel,
     [{"Longest straight paths are grouped as `rest_for_one`",
       ?_assertMatch({rest_for_one, [gen_server1, gen_server2, gen_server3]},
                     gen_server_dependencies:group(#{gen_server1 => [gen_server2],
                                                     gen_server2 => [gen_server3],
                                                     gen_server3 => []}))},
      {"Many neighbours are grouped as `one_for_one`",
       ?_assertMatch({rest_for_one,
                      [{one_for_one, [gen_server5, gen_server1]},
                       gen_server2,
                       {one_for_one, [gen_server4, gen_server3]}]},
                     gen_server_dependencies:group(#{gen_server1 => [gen_server2],
                                                     gen_server2 => [gen_server3, gen_server4],
                                                     gen_server3 => [],
                                                     gen_server4 => [],
                                                     gen_server5 => [gen_server2]}))},
      {"Cyclic strongly connected components are grouped as `one_for_all`",
       ?_assertMatch({rest_for_one,
                      [gen_server1, {one_for_all, [gen_server2, gen_server3, gen_server4]}]},
                     gen_server_dependencies:group(#{gen_server1 => [gen_server2],
                                                     gen_server2 => [gen_server3],
                                                     gen_server3 => [gen_server2, gen_server4],
                                                     gen_server4 => [gen_server3]}))}]}.

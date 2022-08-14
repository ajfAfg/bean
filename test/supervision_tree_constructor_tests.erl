-module(supervision_tree_constructor_tests).

-include_lib("eunit/include/eunit.hrl").

construct_test_() ->
    {inparallel,
     [{"Supervise with `rest_for_one`",
       ?_assertMatch([{sup_spec,
                       _, % Name
                       #{strategy := rest_for_one},
                       [#{id := first_server, type := worker},
                        #{id := second_server, type := worker},
                        #{id := third_server, type := worker}]}],
                     supervision_tree_constructor:construct([c_modules:first_server(),
                                                             c_modules:second_server(),
                                                             c_modules:third_server()]))},
      {"Supervise dependency-free `gen_server`s with `one_for_one`",
       ?_assertMatch([{sup_spec,
                       _, % Name
                       #{strategy := one_for_one},
                       [#{id := third_server, type := worker},
                        #{id := fib_server, type := worker}]}],
                     supervision_tree_constructor:construct([c_modules:fib_server(),
                                                             c_modules:third_server()]))},
      {"Ignore modules other than `gen_server`",
       ?_assertMatch([{sup_spec,
                       _,
                       #{strategy := one_for_one},
                       [#{id := fib_server, type := worker}]}],
                     supervision_tree_constructor:construct([c_modules:fib_server(),
                                                             c_modules:foo()]))}]}.

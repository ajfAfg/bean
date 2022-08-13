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
                                                             c_modules:third_server()]))}]}.

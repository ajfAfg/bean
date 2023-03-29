-module(dependency_extractor_tests).

-include_lib("eunit/include/eunit.hrl").

extract_dependencies_test_() ->
    {inparallel,
     [{"extract `gen_server` dependencies.",
       ?_assertEqual(#{first_server => [second_server],
                       second_server => [third_server],
                       third_server => []},
                     dependency_extractor:extract([c_modules:first_server(),
                                                   c_modules:second_server(),
                                                   c_modules:third_server()]))},
      {"ignore modules other than `gen_server`",
       ?_assertEqual(#{fib_server => []},
                     dependency_extractor:extract([c_modules:fib_server(), c_modules:foo()]))}]}.

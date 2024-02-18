-module(dependency_extractor_tests).

-include_lib("eunit/include/eunit.hrl").

extract_dependencies_test_() ->
    {inparallel,
     [{"extract `gen_server` dependencies.",
       ?_assertEqual({some,
                      #{first_server => [],
                        second_server => [first_server],
                        third_server => [second_server]}},
                     dependency_extractor:extract([c_modules:first_server(),
                                                   c_modules:second_server(),
                                                   c_modules:third_server()]))},
      {"ignore modules other than `gen_server`",
       ?_assertEqual({some, #{fib_server => []}},
                     dependency_extractor:extract([c_modules:fib_server(), c_modules:foo()]))}]}.

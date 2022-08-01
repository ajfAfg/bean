-module(c_gen_server_tests).

-include_lib("eunit/include/eunit.hrl").

is_gen_server_test_() ->
    {inparallel,
     [{"return `true` when passing `gen_server`.",
       ?_assert(c_gen_server:is_gen_server(
                    c_modules:gen_server()))},
      {"return `false` when passing anything other than `gen_server`.",
       ?_assertNot(c_gen_server:is_gen_server(
                       c_modules:foo()))}]}.

-module(c_gen_server_tests).

-include_lib("eunit/include/eunit.hrl").

is_gen_server_test_() ->
    {inparallel,
     [fun return_true_when_passing_gen_server/0,
      fun return_false_when_passing_anything_other_than_gen_server/0]}.

return_true_when_passing_gen_server() ->
    CModule = c_modules:gen_server(),
    ?_assert(c_gen_server:is_gen_server(CModule)).

return_false_when_passing_anything_other_than_gen_server() ->
    CModule = c_modules:foo(),
    ?_assertNot(c_gen_server:is_gen_server(CModule)).

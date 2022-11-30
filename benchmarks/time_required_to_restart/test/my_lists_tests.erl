-module(my_lists_tests).

-include_lib("eunit/include/eunit.hrl").

shuffle_test_() ->
    {inparallel,
     [{"The lists before and after shuffling are the same when sorted.",
       fun() ->
          TestCases = [[], [1], [1, 2, 3]],
          lists:foreach(fun(L) ->
                           ?assertEqual(lists:sort(L),
                                        lists:sort(
                                            my_lists:shuffle(L)))
                        end,
                        TestCases)
       end}]}.

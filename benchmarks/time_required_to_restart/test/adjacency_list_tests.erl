-module(adjacency_list_tests).

-include_lib("eunit/include/eunit.hrl").

create_randomly_test_() ->
    TestCases = [{[1], 0}, {lists:seq(1, 10), 45}, {lists:seq(1, 100), 4950}],
    {inparallel,
     [{"The number of edges is the same as the second parameter.",
       fun() ->
          lists:foreach(fun({Vertices, EdgeNum}) ->
                           ?assertEqual(EdgeNum,
                                        sum(lists:map(fun erlang:length/1,
                                                      maps:values(
                                                          adjacency_list:create_randomly(Vertices,
                                                                                         EdgeNum)))))
                        end,
                        TestCases)
       end},
      {"Edges do not overlap.",
       fun() ->
          lists:foreach(fun({Vertices, EdgeNum}) ->
                           lists:foreach(fun(Tos) -> ?assertEqual(lists:uniq(Tos), Tos) end,
                                         maps:values(
                                             adjacency_list:create_randomly(Vertices, EdgeNum)))
                        end,
                        TestCases)
       end},
      {"There is no loop.",
       fun() ->
          lists:foreach(fun({Vertices, EdgeNum}) ->
                           Graph = adjacency_list:create_randomly(Vertices, EdgeNum),
                           maps:foreach(fun(From, Tos) -> ?assertNot(lists:member(From, Tos)) end,
                                        Graph)
                        end,
                        TestCases)
       end}]}.

sum(NumList) -> lists:foldl(fun erlang:'+'/2, 0, NumList).

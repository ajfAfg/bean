-module(optimum_supervision_tree_solver_tests).

-include_lib("eunit/include/eunit.hrl").

-import(lists, [map/2, sort/1]).

-define(assertExistsIsoTree(ExpectedTrees, ActualTree),
        % NOTE:
        % If a user-defined function is used to determine if two values are equal,
        % the got value is not displayed when the test fails.
        % Therefore, the expected value and got value are explicitly displayed
        % (though the display method is slightly distorted from the usual).
        begin
            V = lists:any(fun(ExpectedTree) ->
                             supervision_tree:are_isomorphic(ExpectedTree, ActualTree)
                          end,
                          ExpectedTrees),
            case V of
                true -> ok;
                false ->
                    ?debugFmt("~nexpected: ~s~nactual: ~p~n",
                              [string:join([io_lib:format("~p", [T]) || T <- ExpectedTrees],
                                           " or\n          "),
                               ActualTree])
            end,
            ?assert(V)
        end).

% NOTE: Tests for critical inputs.
solve_test_() ->
    CreateGraph =
        fun(Vertices, Edges) ->
           lists:foldl(fun({From, To}, Map) ->
                          maps:update_with(From, fun(Tos) -> [To | Tos] end, Map)
                       end,
                       maps:from_keys(Vertices, []),
                       Edges)
        end,
    Solve =
        fun(G) ->
           optimum_supervision_tree_solver:solve(G,
                                                 fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1)
        end,
    {inparallel,
     [{"Standard graph",
       fun() ->
          G = CreateGraph(seq_atom(1, 7),
                          [{'1', '2'}, {'2', '3'}, {'3', '4'}, {'3', '5'}, {'6', '7'}]),
          ?assertExistsIsoTree([{one_for_one,
                                 [{rest_for_one, ['5', '4', '3', '2', '1']},
                                  {rest_for_one, ['7', '6']}]},
                                {one_for_one,
                                 [{rest_for_one, ['4', '5', '3', '2', '1']},
                                  {rest_for_one, ['7', '6']}]}],
                               Solve(G))
       end},
      {"Graph like Lattice",
       fun() ->
          G = CreateGraph(seq_atom(1, 6),
                          [{'1', '3'},
                           {'1', '4'},
                           {'2', '4'},
                           {'2', '5'},
                           {'3', '6'},
                           {'4', '6'},
                           {'5', '6'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['6',
                                  '4',
                                  {one_for_one,
                                   [{rest_for_one, ['3', '1']}, {rest_for_one, ['5', '2']}]}]}],
                               Solve(G))
       end},
      {"Binary tree",
       fun() ->
          G = CreateGraph(seq_atom(1, 7),
                          [{'1', '3'}, {'2', '3'}, {'3', '7'}, {'4', '6'}, {'5', '6'}, {'6', '7'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['7',
                                  {one_for_one,
                                   [{rest_for_one, ['3', {one_for_one, ['1', '2']}]},
                                    {rest_for_one, ['6', {one_for_one, ['4', '5']}]}]}]}],
                               Solve(G))
       end},
      {"Graph having circles",
       fun() ->
          G1 = CreateGraph(seq_atom(1, 5),
                           [{'1', '2'}, {'2', '3'}, {'3', '4'}, {'4', '2'}, {'3', '5'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['5', {one_for_all, ['4', '3', '2', {rest_for_one, ['1']}]}]}],
                               Solve(G1)),
          G2 = CreateGraph(seq_atom(1, 5),
                           [{'1', '2'},
                            {'2', '3'},
                            {'2', '4'},
                            {'2', '5'},
                            {'3', '1'},
                            {'4', '5'}]),
          ?assertExistsIsoTree([{rest_for_one, ['5', '4', {one_for_all, ['3', '2', '1']}]}],
                               Solve(G2)),
          G3 = CreateGraph(seq_atom(1, 4),
                           [{'1', '2'}, {'2', '1'}, {'2', '3'}, {'3', '4'}, {'4', '3'}]),
          ?assertExistsIsoTree([{one_for_all, ['3', '4', {one_for_all, ['1', '2']}]}], Solve(G3)),
          G4 = CreateGraph(seq_atom(1, 8),
                           [{'1', '2'},
                            {'2', '1'},
                            {'2', '3'},
                            {'3', '4'},
                            {'4', '5'},
                            {'5', '6'},
                            {'6', '5'},
                            {'6', '8'},
                            {'7', '8'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['8',
                                  {one_for_one,
                                   ['7',
                                    {one_for_all,
                                     ['5',
                                      '6',
                                      {rest_for_one, ['4', '3', {one_for_all, ['1', '2']}]}]}]}]}],
                               Solve(G4)),
          G5 = CreateGraph(seq_atom(1, 6),
                           [{'1', '3'},
                            {'2', '3'},
                            {'3', '4'},
                            {'4', '5'},
                            {'5', '4'},
                            {'5', '6'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['6',
                                  {one_for_all,
                                   ['5', '4', {rest_for_one, ['3', {one_for_one, ['2', '1']}]}]}]}],
                               Solve(G5))
       end},
      {"Graph having circles in first and last",
       fun() ->
          G = CreateGraph(seq_atom(1, 6),
                          [{'1', '2'},
                           {'2', '3'},
                           {'3', '1'},
                           {'3', '4'},
                           {'4', '5'},
                           {'5', '6'},
                           {'6', '5'}]),
          ?assertExistsIsoTree([{one_for_all,
                                 ['6',
                                  '5',
                                  {rest_for_one, ['4', {one_for_all, ['3', '2', '1']}]}]}],
                               Solve(G))
       end},
      {"Cycle graph",
       fun() ->
          G = CreateGraph(seq_atom(1, 4),
                          [{'1', '2'}, {'2', '3'}, {'2', '4'}, {'3', '1'}, {'4', '1'}]),
          ?assertExistsIsoTree([{one_for_all, ['3', '4', '2', '1']}], Solve(G))
       end},
      {"Graph with detour",
       fun() ->
          G = CreateGraph(seq_atom(1, 4), [{'1', '2'}, {'1', '4'}, {'2', '3'}, {'3', '4'}]),
          ?assertExistsIsoTree([{rest_for_one, ['4', '3', '2', '1']}], Solve(G))
       end},
      {"Null graph",
       fun() ->
          G = CreateGraph(seq_atom(1, 3), []),
          ?assertExistsIsoTree([{one_for_one, ['1', '2', '3']}], Solve(G))
       end},
      {"Graph where vertices in the split vertices are not necessarily connected to each other",
       fun() ->
          G = CreateGraph(seq_atom(1, 7),
                          [{'1', '3'},
                           {'1', '5'},
                           {'2', '3'},
                           {'2', '4'},
                           {'2', '7'},
                           {'4', '3'},
                           {'4', '5'},
                           {'4', '7'},
                           {'5', '6'}]),
          ?assertExistsIsoTree([{rest_for_one,
                                 ['6',
                                  '5',
                                  '3',
                                  {one_for_one, ['1', {rest_for_one, ['7', '4', '2']}]}]},
                                {rest_for_one,
                                 ['3',
                                  '6',
                                  '5',
                                  {one_for_one, ['1', {rest_for_one, ['7', '4', '2']}]}]}],
                               Solve(G))
       end}]}.

%% ===================================================================
%% Private API
%% ===================================================================
-spec seq_atom(integer(), integer()) -> [integer()].
seq_atom(From, To) -> [list_to_atom(integer_to_list(X)) || X <- lists:seq(From, To)].

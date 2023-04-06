-module(my_sets_tests).

-include_lib("eunit/include/eunit.hrl").

map_test_() ->
    {inparallel,
     [{"The first law of Functor Laws: Preservation of identity morphisms",
       fun() ->
          Id = fun(X) -> X end,
          lists:foreach(fun(Set) -> ?assertEqual(Set, my_sets:map(Id, Set)) end,
                        [sets:from_list(L) || L <- [[], [1], [1, 2, 3]]])
       end},
      {"The second law of Functor Laws: Preservation of composition of morphisms",
       fun() ->
          Comp = fun(F, G) -> fun(X) -> F(G(X)) end end,
          F = fun(X) -> X + 1 end,
          G = fun(X) -> X * 2 end,
          lists:foreach(fun(Set) ->
                           ?assertEqual(my_sets:map(Comp(F, G), Set),
                                        my_sets:map(F, my_sets:map(G, Set)))
                        end,
                        [sets:from_list(L) || L <- [[], [1], [1, 2, 3]]])
       end}]}.

any_test_() ->
    {inparallel,
     [{"Returns `true` if there is at least one element for which the predicate is `true`, otherwise returns `false`",
       fun() ->
          ?assertEqual(false, my_sets:any(fun(_) -> true end, sets:from_list([]))),
          ?assertEqual(true, my_sets:any(fun(_) -> true end, sets:from_list([1]))),
          ?assertEqual(false, my_sets:any(fun(_) -> false end, sets:from_list([]))),
          ?assertEqual(false, my_sets:any(fun(_) -> false end, sets:from_list([1]))),
          ?assertEqual(true, my_sets:any(fun(X) -> X =:= 1 end, sets:from_list([1]))),
          ?assertEqual(false, my_sets:any(fun(X) -> X =:= 1 end, sets:from_list([2])))
       end}]}.

equal_test_() ->
    {inparallel,
     [{"Return `true` for all sets s, s', if s - s' = ∅ where |s| ≥ |s'|, otherwise return `false`",
       fun() ->
          lists:foreach(fun({S1, S2}) ->
                           Equal =
                               fun(Set1, Set2) ->
                                  {Set1_, Set2_} =
                                      case sets:size(Set1) >= sets:size(Set2) of
                                          true -> {Set1, Set2};
                                          false -> {Set2, Set1}
                                      end,
                                  sets:to_list(
                                      sets:subtract(Set1_, Set2_))
                                  =:= []
                               end,
                           ?assertEqual(Equal(S1, S2), my_sets:equal(S1, S2))
                        end,
                        lists:map(fun({L1, L2}) -> {sets:from_list(L1), sets:from_list(L2)} end,
                                  [{[], []},
                                   {[], [1]},
                                   {[1], []},
                                   {[1], [1]},
                                   {[1, 2], [1, 2]},
                                   {[1, 2], [2, 1]}]))
       end}]}.

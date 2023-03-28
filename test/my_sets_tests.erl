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

foreach_test_() ->
    {inparallel,
     [{"Returns the atom `ok`",
       fun() ->
          ?assertEqual(ok, my_sets:foreach(fun(X) -> X end, sets:from_list([]))),
          ?assertEqual(ok, my_sets:foreach(fun(X) -> X end, sets:from_list([1, 2, 3])))
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

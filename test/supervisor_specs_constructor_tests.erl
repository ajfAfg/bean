-module(supervisor_specs_constructor_tests).

-include_lib("eunit/include/eunit.hrl").

construct_test_() ->
    SupervisionTrees = {rest_for_one, [foo, {rest_for_one, [bar, {rest_for_one, [baz]}]}]},
    {inparallel,
     [{"The Return value type matches the `supervisor_spec:t()` type",
       fun() ->
          lists:foreach(fun(SupSpec) ->
                           ?assertMatch(#{name := Name,
                                          sup_flags := #{strategy := _},
                                          child_specs := [_ | _]} when is_atom(Name),
                                        SupSpec)
                        end,
                        supervisor_specs_constructor:construct(SupervisionTrees))
       end},
      {"The `child_specs` in the return value matches the `[supervisor:child_spec()]` type",
       fun() ->
          _ = [?assertMatch(#{id := _,
                              start := _,
                              type := _},
                            Spec)
               || #{child_specs := ChildSpecs}
                      <- supervisor_specs_constructor:construct(SupervisionTrees),
                  Spec <- ChildSpecs]
       end}]}.

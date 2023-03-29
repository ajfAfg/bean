-module(supervisor_specs_constructor_tests).

-include_lib("eunit/include/eunit.hrl").

% NOTE:
% Extracting dependencies from `gen_server` is tested in `dependency_extractor_tests.erl`,
% and generating the supervision tree from the dependencies is tested in `optimum_supervision_tree_solver_tests`.
% Therefore, it is sufficient to this test only for type consistency.
construct_test_() ->
    {inparallel,
     [{"The Return value type matches the `supervisor_spec:t()` type",
       fun() ->
          SupSpecs =
              supervisor_specs_constructor:construct([c_modules:first_server(),
                                                      c_modules:second_server(),
                                                      c_modules:third_server()]),
          lists:foreach(fun(SupSpec) ->
                           ?assertMatch(#{name := Name,
                                          sup_flags := #{strategy := _},
                                          child_specs := [_ | _]} when is_atom(Name),
                                        SupSpec)
                        end,
                        SupSpecs)
       end},
      {"The `child_specs` in the return value matches the `[supervisor:child_spec()]` type",
       fun() ->
          [#{child_specs := ChildSpecs} | _] =
              supervisor_specs_constructor:construct([c_modules:first_server(),
                                                      c_modules:second_server(),
                                                      c_modules:third_server()]),
          lists:foreach(fun(Spec) ->
                           ?assertMatch(#{id := _,
                                          start := _,
                                          type := _},
                                        Spec)
                        end,
                        ChildSpecs)
       end}]}.

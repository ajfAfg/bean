-module(supervision_tree_constructor_tests).

-include_lib("eunit/include/eunit.hrl").

% NOTE:
% Extracting dependencies from `gen_server` is tested in `gen_server_dependencies_tests.erl`,
% and generating the supervision tree from the dependencies is tested in `optimum_supervision_tree_solver_tests`.
% Therefore, it is sufficient to this test only for type consistency.
construct_test_() ->
    {inparallel,
     [{"The Return value type matches the `sup_spec` type",
       fun() ->
          SupSpecs =
              supervision_tree_constructor:construct([c_modules:first_server(),
                                                      c_modules:second_server(),
                                                      c_modules:third_server()]),
          lists:foreach(fun(SupSpec) ->
                           ?assertMatch({sup_spec, Name, #{strategy := _}, [_ | _]} when is_atom(Name), SupSpec)
                        end,
                        SupSpecs)
       end},
      {"The Children in the return value matches the `supervisor:child_spec` type",
       fun() ->
          [{_, _, _, Children} | _] =
              supervision_tree_constructor:construct([c_modules:first_server(),
                                                      c_modules:second_server(),
                                                      c_modules:third_server()]),
          lists:foreach(fun(Child) ->
                           ?assertMatch(#{id := _,
                                          start := _,
                                          type := _},
                                        Child)
                        end,
                        Children)
       end}]}.

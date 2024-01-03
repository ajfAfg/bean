-module(prop_optimum_supervision_tree_solver).

-compile(export_all).

-import(proper_helper, [limited_atom/0, random_type/0]).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% optimum_supervision_tree_solver:solve/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_solve1(doc) ->
    "When a process is restarted, the processes that depend on it are also restarted".

prop_solve1() ->
    ?FORALL(Graph,
            dependency_graph_generator:dependency_graph(),
            begin
                Digraph = dependency_digraph:from_dependency_graph(Graph),
                lists:all(fun(V) ->
                             digraph_utils:reaching([V], Digraph)
                             -- take_restart_processes(V,
                                                       optimum_supervision_tree_solver:solve(Graph,
                                                                                             fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1))
                             =:= []
                          end,
                          digraph:vertices(Digraph))
            end).

take_restart_processes(Target, {Strategy, Children} = Tree) ->
    case lists:member(Target, Children) of
        false ->
            lists:flatten(
                lists:map(fun(Child) -> take_restart_processes(Target, Child) end, Children));
        true ->
            case Strategy of
                one_for_one -> [Target];
                one_for_all -> take_restart_processes(Tree);
                rest_for_one ->
                    {_, Children_} = lists:splitwith(fun(C) -> C =/= Target end, Children),
                    lists:flatten(
                        lists:map(fun take_restart_processes/1, Children_))
            end
    end;
take_restart_processes(_, _) -> [].

take_restart_processes({_, Children}) ->
    lists:flatten(
        lists:map(fun take_restart_processes/1, Children));
take_restart_processes(Child) -> Child.

prop_solve2(doc) -> "About cost, exp time & correct ≤ polynomial time & incorrect".

prop_solve2() ->
    ?FORALL(Graph,
            dependency_graph_generator:dependency_graph(),
            supervision_tree:calc_cost(
                optimum_supervision_tree_solver:solve(Graph,
                                                      fun all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness/1))
            =< supervision_tree:calc_cost(
                   optimum_supervision_tree_solver:solve(Graph,
                                                         fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1))).

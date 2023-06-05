-module(prop_optimum_supervision_tree_solver).

-compile(export_all).

-import(proper_helper, [limited_atom/0]).

-include_lib("proper/include/proper.hrl").

dependency_graph() ->
    ?LET(List,
         non_empty(list(limited_atom())),
         begin
             Vertices = lists:uniq(List),
             lists:foldl(fun(From, Acc) ->
                            Tos = my_lists:sublist_randomly(Vertices),
                            maps:put(From, Tos, Acc)
                         end,
                         #{},
                         Vertices)
         end).

dependency_graph_to_connected_dag(G) ->
    Digraph =
        digraph_utils:condensation(
            dependency_digraph:from_dependency_graph(G)),
    digraph_utils:subgraph(Digraph, hd(digraph_utils:components(Digraph))).

dependency_connected_dag() ->
    {'$call', ?MODULE, dependency_graph_to_connected_dag, [dependency_graph()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% optimum_supervision_tree_solver:solve/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_solve(doc) ->
    "When a process is restarted, the processes that depend on it are also restarted".

prop_solve() ->
    ?FORALL(Graph,
            dependency_graph(),
            begin
                Digraph = dependency_digraph:from_dependency_graph(Graph),
                lists:all(fun(V) ->
                             digraph_utils:reaching([V], Digraph)
                             -- take_restart_processes(V,
                                                       optimum_supervision_tree_solver:solve(Graph))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% optimum_supervision_tree_solver:take_split_vertices/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_take_split_vertices(doc) ->
    "Satisfies the definition of *split vertices* except for *minimal*".

prop_take_split_vertices() ->
    ?FORALL(ConnectedDAG,
            dependency_connected_dag(),
            begin
                SubGraph =
                    digraph_utils:subgraph(
                        my_digraph_utils:clone(ConnectedDAG),
                        optimum_supervision_tree_solver:take_split_vertices(ConnectedDAG)),
                NextConnectedDAGs =
                    lists:map(fun(C) ->
                                 digraph_utils:subgraph(
                                     my_digraph_utils:clone(SubGraph), C)
                              end,
                              digraph_utils:components(SubGraph)),
                lists:all(fun(V) -> V end,
                          [not my_digraph:has_path(ConnectedDAG, V1, V2)
                           || G1 <- NextConnectedDAGs,
                              G2 <- NextConnectedDAGs,
                              G1 =/= G2,
                              V1 <- digraph:vertices(G1),
                              V2 <- digraph:vertices(G2)])
            end).

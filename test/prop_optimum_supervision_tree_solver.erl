-module(prop_optimum_supervision_tree_solver).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

dependency_graph() ->
    ?LET(List,
         non_empty(list(atom())),
         begin
             Vertices = lists:uniq(List),
             lists:foldl(fun(From, Acc) ->
                            Tos = my_lists:sublist_randomly(Vertices),
                            maps:put(From, Tos, Acc)
                         end,
                         #{},
                         Vertices)
         end).

dependency_digraph() ->
    {'$call', dependency_digraph, from_dependency_graph, [dependency_graph()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% optimum_supervision_tree_solver:group/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_group1(doc) -> "No vertex is lost".

prop_group1() ->
    ?FORALL(Graph,
            dependency_digraph(),
            begin
                GroupedGraph = optimum_supervision_tree_solver:group(Graph),
                lists:sort(
                    digraph:vertices(Graph))
                =:= lists:sort(
                        lists:flatten([sets:to_list(S) || S <- digraph:vertices(GroupedGraph)]))
            end).

prop_group2(doc) -> "No edge is lost".

prop_group2() ->
    ?FORALL(Graph,
            dependency_digraph(),
            begin
                GroupedGraph = optimum_supervision_tree_solver:group(Graph),
                lists:all(fun({GV1, GV2}) ->
                             lists:any(fun({V1, V2}) -> my_digraph:has_path(Graph, V1, V2) end,
                                       [{V1, V2}
                                        || V1 <- sets:to_list(GV1), V2 <- sets:to_list(GV2)])
                          end,
                          [{GV1, GV2}
                           || GE <- digraph:edges(GroupedGraph),
                              {_, GV1, GV2, _} <- [digraph:edge(GroupedGraph, GE)]])
            end).

prop_group3(doc) -> "Return a different instance from the given graph".

prop_group3() ->
    ?FORALL(Graph,
            dependency_digraph(),
            Graph =/= optimum_supervision_tree_solver:group(Graph)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% optimum_supervision_tree_solver:transform/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_transform1(doc) ->
    "The strategy of the parent of the vertex contained in a cyclic strongly component in the graph is `one_for_all`".

prop_transform1() ->
    ?FORALL(Graph,
            dependency_digraph(),
            begin
                Tree =
                    optimum_supervision_tree_solver:transform(Graph,
                                                              optimum_supervision_tree_solver:group(Graph)),
                ?FORALL({Strategy, Children},
                        exactly(hd(my_lists:shuffle(take_strategy_and_names(Tree)))),
                        ?FORALL(Child,
                                exactly(hd(my_lists:shuffle(Children))),
                                begin
                                    Cscc =
                                        my_digraph_utils:get_cyclic_strong_component(Graph, Child),
                                    ?IMPLIES(is_list(Cscc) andalso Cscc -- Children =:= [],
                                             Strategy =:= one_for_all)
                                end))
            end).

take_strategy_and_names({Strategy, Children}) ->
    Names = [Name || Name <- Children, is_atom(Name)],
    Trees = [Tree || Tree <- Children, not is_atom(Tree)],
    V = case Names of
            [] -> [];
            _ -> [{Strategy, Names}]
        end,
    V
    ++ lists:flatten(
           lists:map(fun take_strategy_and_names/1, Trees)).

prop_transform2(doc) ->
    "The order of the children of a supervisor with the strategy `rest_for_one follows postorder".

prop_transform2() ->
    ?FORALL(Graph,
            dependency_digraph(),
            begin
                Tree =
                    optimum_supervision_tree_solver:transform(Graph,
                                                              optimum_supervision_tree_solver:group(Graph)),
                ChildrenDescendantPairList =
                    take_children_descendant_pair_list_about_rest_for_one(Tree),
                lists:all(fun({Children, Descendant}) ->
                             lists:all(fun({V1, V2}) ->
                                          my_digraph:has_path(Graph, V2, V1)
                                          orelse not my_digraph:has_path(Graph, V1, V2)
                                       end,
                                       [{V1, V2} || V1 <- Children, V2 <- Descendant])
                          end,
                          ChildrenDescendantPairList)
            end).

take_children_descendant_pair_list_about_rest_for_one({Strategy, Children}) ->
    Names = [Name || Name <- Children, is_atom(Name)],
    Trees = [Tree || Tree <- Children, not is_atom(Tree)],
    V = case Strategy of
            rest_for_one ->
                [{Names,
                  lists:flatten(
                      lists:map(fun take_names/1, Trees))}];
            _ -> []
        end,
    lists:foldl(fun(L, Acc) -> Acc ++ L end,
                V,
                lists:map(fun take_children_descendant_pair_list_about_rest_for_one/1, Trees)).

take_names(Name) when is_atom(Name) -> Name;
take_names({_, Children}) ->
    lists:flatten(
        lists:map(fun take_names/1, Children)).

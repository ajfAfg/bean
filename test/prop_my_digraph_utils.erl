-module(prop_my_digraph_utils).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

vertices_and_edges() ->
    ?LET(List,
         % NOTE:
         % I want to be sure to retrieve an element from the list during testing,
         % so I do not want an empty list.
         non_empty(list(any())),
         begin
             Vertices = lists:usort(List),
             {Vertices, create_edges_randomly(Vertices)}
         end).

create_edges_randomly(Vertices) ->
    Vs = fun() -> my_lists:sublist_randomly(Vertices) end,
    [{V, U} || V <- Vs(), U <- Vs()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_digraph_utils:get_strong_component/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_get_strong_component(doc) ->
    "If the return value is a list, it satisfies the conditions of a strongly connected component".

prop_get_strong_component() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph:create(Vertices, Edges),
                ?FORALL(Vertex,
                        exactly(hd(my_lists:shuffle(Vertices))),
                        begin
                            SCC = my_digraph_utils:get_strong_component(G, Vertex),
                            ?IMPLIES(is_list(SCC),
                                     lists:all(fun ({V, V}) -> true;
                                                   ({V1, V2}) -> my_digraph:has_path(G, V1, V2)
                                               end,
                                               [{V1, V2} || V1 <- SCC, V2 <- SCC]))
                        end)
            end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_digraph_utils:get_cyclic_strong_component/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_get_cyclic_strong_component(doc) ->
    "If the return value is a list, it satisfies the conditions of a cyclic strongly connected component".

prop_get_cyclic_strong_component() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph:create(Vertices, Edges),
                ?FORALL(Vertex,
                        exactly(hd(my_lists:shuffle(Vertices))),
                        begin
                            CSCC = my_digraph_utils:get_cyclic_strong_component(G, Vertex),
                            ?IMPLIES(is_list(CSCC),
                                     lists:all(fun({V1, V2}) -> my_digraph:has_path(G, V1, V2) end,
                                               [{V1, V2} || V1 <- CSCC, V2 <- CSCC]))
                        end)
            end).

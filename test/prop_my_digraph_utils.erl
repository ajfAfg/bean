-module(prop_my_digraph_utils).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

vertices_and_edges() ->
    ?LET(List,
         % NOTE:
         % I want to be sure to retrieve an element from the list during testing,
         % so I do not want an empty list.
         non_empty(list(random_type())),
         begin
             Vertices = lists:usort(List),
             {Vertices, create_edges_randomly(Vertices)}
         end).

create_edges_randomly(Vertices) ->
    Vs = fun() -> my_lists:sublist_randomly(Vertices) end,
    [{V, U} || V <- Vs(), U <- Vs()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_digraph_utils:clone/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_clone1(doc) ->
    "The instances of the argument graph and the return graph are different".

prop_clone1() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph:create(Vertices, Edges),
                G =/= my_digraph_utils:clone(G)
            end).

prop_clone2(doc) ->
    "The instances of the argument graph and the return graph have equal vertices and edges".

prop_clone2() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph_utils:clone(
                        my_digraph:create(Vertices, Edges)),
                lists:sort(Vertices)
                =:= lists:sort(
                        digraph:vertices(G))
                andalso lists:sort(Edges) =:= lists:sort(edges(G))
            end).

-spec edges(digraph:graph()) -> [{digraph:vertex(), digraph:vertex()}].
edges(Graph) ->
    [{V1, V2} || Edge <- digraph:edges(Graph), {_, V1, V2, _} <- [digraph:edge(Graph, Edge)]].

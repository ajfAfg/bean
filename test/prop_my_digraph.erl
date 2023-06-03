-module(prop_my_digraph).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

vertices_and_edges() ->
    ?LET(List,
         list(random_type()),
         begin
             Vertices = lists:usort(List),
             {Vertices, create_edges_randomly(Vertices)}
         end).

create_edges_randomly(Vertices) ->
    Vs = fun() -> my_lists:sublist_randomly(Vertices) end,
    [{V, U} || V <- Vs(), U <- Vs()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_digraph:create/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_create(doc) ->
    "The vertex and edge sets used to create the graph are indeed included in the graph".

prop_create() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph:create(Vertices, Edges),
                lists:sort(Vertices)
                =:= lists:sort(
                        digraph:vertices(G))
                andalso lists:sort(Edges)
                        =:= lists:sort([{V, U}
                                        || E <- digraph:edges(G),
                                           {_, V, U, _} <- [digraph:edge(G, E)]])
            end).

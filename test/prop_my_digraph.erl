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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% my_digraph:has_path/3 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% Although I should test whether the path definition is satisfied,
% I cannot test it with realistic resources,
% so I test by "Wording the specification differently".
prop_has_path(doc) -> "Wording the specification differently".

prop_has_path() ->
    ?FORALL({Vertices, Edges},
            vertices_and_edges(),
            begin
                G = my_digraph:create(Vertices, Edges),
                lists:all(fun({V1, V2}) -> has_path(G, V1, V2) =:= my_digraph:has_path(G, V1, V2)
                          end,
                          [{V1, V2} || V1 <- Vertices, V2 <- Vertices])
            end).

has_path(G, V1, V2) ->
    MutableSet = ets:new(set, [set, private]),
    V = has_path(G, V1, V2, MutableSet),
    ets:delete(MutableSet),
    V.

has_path(G, V1, V2, Visited) ->
    case ets:lookup(Visited, V1) of
        [] ->
            Neighbors = digraph:out_neighbours(G, V1),
            case lists:member(V2, Neighbors) of
                true -> true;
                false ->
                    ets:insert(Visited, {V1}),
                    lists:any(fun(V) -> has_path(G, V, V2, Visited) end, Neighbors)
            end;
        [_ | _] -> false
    end.

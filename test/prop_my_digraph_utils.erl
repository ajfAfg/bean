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

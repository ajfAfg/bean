-module(my_digraph).

-export([union/3, get_longest_straight_path/2, create/2, has_path/3]).

-spec union(digraph:graph(), [digraph:vertex()], digraph:vertex()) -> digraph:vertex().
union(Graph, Vertices, NewVertex) ->
    digraph:add_vertex(Graph, NewVertex),
    {Froms, Tos} =
        lists:foldl(fun(Edge, {Froms, Tos} = Acc) ->
                       {Edge, From, To, _} = digraph:edge(Graph, Edge),
                       case {lists:member(From, Vertices), lists:member(To, Vertices)} of
                           {true, true} -> Acc;
                           {true, false} -> {Froms, [To | Tos]};
                           {false, true} -> {[From | Froms], Tos};
                           {false, false} -> Acc
                       end
                    end,
                    {[], []},
                    digraph:edges(Graph)),
    lists:foreach(fun(From) -> digraph:add_edge(Graph, From, NewVertex) end,
                  lists:uniq(Froms)),
    lists:foreach(fun(To) -> digraph:add_edge(Graph, NewVertex, To) end, lists:uniq(Tos)),
    digraph:del_vertices(Graph, Vertices),
    NewVertex.

% NOTE:
% I believe that if a path does not exist, an empty list should be returned,
% but since, for example, `digraph:get_path/3` returns `false` in such a case,
% this function also follows suit.
-spec get_longest_straight_path(digraph:graph(), digraph:vertex()) ->
                                   [digraph:vertex()] | false.
get_longest_straight_path(Graph, Vertex) ->
    case digraph:vertex(Graph, Vertex) of
        false -> false;
        _ ->
            case get_longest_straight_path_aux(Graph, Vertex) of
                [] -> false;
                [_] -> false;
                Path -> Path
            end
    end.

get_longest_straight_path_aux(Graph, Vertex) ->
    case digraph:in_degree(Graph, Vertex) =< 1 andalso digraph:out_degree(Graph, Vertex) =< 1
    of
        true ->
            case digraph:out_neighbours(Graph, Vertex) of
                [] -> [Vertex];
                [NextVertex] -> [Vertex | get_longest_straight_path_aux(Graph, NextVertex)];
                _ -> []
            end;
        false -> []
    end.

% TODO: Support a labeled vertex and a labeled edge
-spec create([digraph:vertex()], [{digraph:vertex(), digraph:vertex()}]) ->
                digraph:graph().
create(Vertices, Edges) ->
    G = digraph:new(),
    lists:foreach(fun(X) -> digraph:add_vertex(G, X) end, Vertices),
    lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end, Edges),
    G.

-spec has_path(digraph:graph(), digraph:vertex(), digraph:graph()) -> boolean().
has_path(G, V1, V2) ->
    case digraph:get_path(G, V1, V2) of
        false -> false;
        _ -> true
    end.

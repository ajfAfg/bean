-module(my_digraph).

-export([create/2, has_path/3]).

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

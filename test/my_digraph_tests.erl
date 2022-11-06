-module(my_digraph_tests).

-include_lib("eunit/include/eunit.hrl").

union_test_() ->
    {inparallel,
     [{"Union the vertices to the new vertex",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c]),
          my_digraph:union(G, [a, b, c], d),
          ?assertEqual([d], digraph:vertices(G))
       end},
      {"Append the edges of the vertices to be deleted to the new vertex",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c, d]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end, [{a, b}, {b, c}, {c, d}]),
          my_digraph:union(G, [b, c], e),
          ?assertEqual([a, e, d], digraph:get_path(G, a, d))
       end},
      {"The new vertex may be an already existing vertex",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end, [{a, b}, {b, c}]),
          my_digraph:union(G, [b], a),
          ?assertEqual([a, c], digraph:get_path(G, a, c))
       end},
      {"Multiple edges do not exist",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c, d]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{a, b}, {a, c}, {b, d}, {c, d}]),
          my_digraph:union(G, [b, c], e),
          ?assertEqual(1, digraph:in_degree(G, e)),
          ?assertEqual(1, digraph:out_degree(G, e))
       end}]}.

get_longest_straight_path_test_() ->
    {inparallel,
     [{"Return a longest straight path",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end, [{a, b}, {b, c}]),
          ?assertEqual([a, b, c], my_digraph:get_longest_straight_path(G, a))
       end},
      {"Return `false` if a longest straight path does not exist",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end, [{a, b}, {a, c}]),
          ?assertEqual(false, my_digraph:get_longest_straight_path(G, a))
       end},
      {"Stop on the way",
       fun() ->
          G = digraph:new(),
          lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, [a, b, c, d, e, f, g, h]),
          lists:foreach(fun({V1, V2}) -> digraph:add_edge(G, V1, V2) end,
                        [{a, c}, {b, c}, {c, d}, {d, e}, {e, f}, {f, g}, {f, h}]),
          ?assertEqual([d, e], my_digraph:get_longest_straight_path(G, d))
       end},
      {"Return `false` if a non-existent vertex is given",
       fun() ->
          G = digraph:new(),
          ?assertEqual(false, my_digraph:get_longest_straight_path(G, a))
       end},
      {"Return `false` because one vertex path is not allowed",
       fun() ->
          G = digraph:new(),
          digraph:add_vertex(G, a),
          ?assertEqual(false, my_digraph:get_longest_straight_path(G, a))
       end}]}.

create_test_() ->
    {inparallel,
     [{"Return a correct graph",
       fun() ->
          Vertices = [1, 2, 3, 4],
          Edges = [{1, 2}, {1, 3}, {2, 4}, {3, 4}],
          G = my_digraph:create(Vertices, Edges),
          ?assertEqual(lists:sort(Vertices),
                       lists:sort(
                           digraph:vertices(G))),
          ?assertEqual(lists:sort(Edges),
                       lists:sort(
                           lists:map(fun(E) ->
                                        {E, V1, V2, _} = digraph:edge(G, E),
                                        {V1, V2}
                                     end,
                                     digraph:edges(G))))
       end}]}.

has_path_test_() ->
    {inparallel,
     [{"Return `true` if the given graph has a path",
       ?_assert(my_digraph:has_path(
                    my_digraph:create([1, 2], [{1, 2}]), 1, 2))},
      {"Return `false` if the given graph has no path",
       ?_assertNot(my_digraph:has_path(
                       my_digraph:create([1, 2], []), 1, 2))}]}.

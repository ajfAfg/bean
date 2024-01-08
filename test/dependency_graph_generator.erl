-module(dependency_graph_generator).

-export([dependency_graph/1]).

-import(proper_helper, [limited_atom/0]).

-include_lib("proper/include/proper.hrl").

dependency_graph(MaxVertexNum) ->
    ?LET(List,
         ?SUCHTHAT(L, non_empty(list(limited_atom())), length(L) =< MaxVertexNum),
         begin
             Vertices = lists:uniq(List),
             MaximumValidEdges = [{V1, V2} || V1 <- Vertices, V2 <- Vertices],
             % NOTE: When the number of vertices is the same as the number of edges,
             % it often generates a graph that are complex to compute vertex splitters,
             % in my experience.
             Edges =
                 lists:nthtail(length(MaximumValidEdges) - length(Vertices),
                               my_lists:shuffle(MaximumValidEdges)),
             lists:foldl(fun({V1, V2}, Acc) -> maps:update_with(V1, fun(Vs) -> [V2 | Vs] end, Acc)
                         end,
                         maps:from_keys(Vertices, []),
                         Edges)
         end).

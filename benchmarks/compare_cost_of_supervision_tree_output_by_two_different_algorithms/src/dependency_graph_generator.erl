-module(dependency_graph_generator).

-export([generate_randomly/2]).

-spec generate_randomly(non_neg_integer(), non_neg_integer()) -> dependency_graph:t().
generate_randomly(VertexNum, EdgeNum)
    when 0 =< VertexNum andalso 0 =< EdgeNum andalso EdgeNum =< VertexNum * (VertexNum - 1) ->
    Vertices = [list_to_atom(integer_to_list(X)) || X <- lists:seq(1, VertexNum)],
    MaximumValidEdges = [{V1, V2} || V1 <- Vertices, V2 <- Vertices, V1 =/= V2],
    Edges =
        lists:nthtail(length(MaximumValidEdges) - EdgeNum, my_lists:shuffle(MaximumValidEdges)),
    lists:foldl(fun({V1, V2}, Acc) -> maps:update_with(V1, fun(Vs) -> [V2 | Vs] end, Acc) end,
                maps:from_keys(Vertices, []),
                Edges).

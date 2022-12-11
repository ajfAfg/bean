-module(adjacency_list).

-export([create_randomly/2]).

-export_type([t/0, vertex/0]).

-type t() :: #{vertex() => vertex()}.
-type vertex() :: term().

-spec create_randomly([vertex()], non_neg_integer()) -> t().
create_randomly(Vertices, EdgeNum)
    when 0 =< EdgeNum andalso EdgeNum =< length(Vertices) * (length(Vertices) - 1) ->
    MaximumValidEdges = [{V1, V2} || V1 <- Vertices, V2 <- Vertices, V1 =/= V2],
    Edges =
        lists:nthtail(length(MaximumValidEdges) - EdgeNum, my_lists:shuffle(MaximumValidEdges)),
    lists:foldl(fun({V1, V2}, Acc) -> maps:update_with(V1, fun(Vs) -> [V2 | Vs] end, Acc) end,
                maps:from_keys(Vertices, []),
                Edges).

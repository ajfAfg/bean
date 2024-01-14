-module(parameters).

-export([vertex_num_and_edge_num_list/0, trial_num/0]).

vertex_num_and_edge_num_list() ->
    [{VertexNum, EdgeNum}
     || VertexNum <- lists:seq(10, 80, 10),
        EdgeNum <- lists:map(fun(X) -> VertexNum * X end, [1, 2, 3])].

trial_num() -> 20.

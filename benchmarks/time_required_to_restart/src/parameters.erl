-module(parameters).

-export([get_vertex_num_and_edge_num/1, get_delay_time/1, get_loop_num/0]).

-type vertex_num() :: pos_integer().
-type edge_num() :: non_neg_integer().

-spec get_vertex_num_and_edge_num(pos_integer()) -> {vertex_num(), edge_num()}.
% NOTE:
% The number of vertices: 5 and 10 times based on 10
% The number of edges: 1 -- 3 times based on the number of vertices, and the maximum number of edges
get_vertex_num_and_edge_num(N) ->
    lists:nth(N,
              [{10, 10},
               {10, 20},
               {10, 30},
               {10, 90},
               {50, 50},
               {50, 100},
               {50, 150},
               {50, 2450},
               {100, 100},
               {100, 200},
               {100, 300},
               {100, 9900}]).

-spec get_delay_time(pos_integer()) -> pos_integer().
get_delay_time(N) -> N * 10.

get_loop_num() -> 50.

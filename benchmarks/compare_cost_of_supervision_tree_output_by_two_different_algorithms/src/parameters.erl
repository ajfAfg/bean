-module(parameters).

-export([vertex_num_and_edge_num_list/0, trial_num/0]).

vertex_num_and_edge_num_list() ->
    [{10, 10},
     {10, 20},
     {10, 30},
     {10, 90},
     {15, 15},
     {15, 30},
     {15, 45},
     {15, 210},
     {20, 20},
     {20, 40},
     {20, 60},
     {20, 380}].

trial_num() -> 20.

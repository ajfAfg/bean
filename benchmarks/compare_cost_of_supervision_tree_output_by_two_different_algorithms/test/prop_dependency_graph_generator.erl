-module(prop_dependency_graph_generator).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

vertex_num_and_edge_num() ->
    ?SUCHTHAT({VertexNum, EdgeNum},
              {pos_integer(), pos_integer()},
              EdgeNum =< VertexNum * (VertexNum - 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% dependency_graph_generator:generate_randomly/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_generate_randomly1(doc) ->
    "The number of the vertices of the return value equals the argument `VertexNum`".

prop_generate_randomly1() ->
    ?FORALL({VertexNum, EdgeNum},
            vertex_num_and_edge_num(),
            begin
                VertexNum
                =:= length(maps:keys(
                               dependency_graph_generator:generate_randomly(VertexNum, EdgeNum)))
            end).

prop_generate_randomly2(doc) ->
    "The number of the edges of the return value equals the argument `EdgeNum`".

prop_generate_randomly2() ->
    ?FORALL({VertexNum, EdgeNum},
            vertex_num_and_edge_num(),
            begin
                EdgeNum
                =:= lists:sum(
                        lists:map(fun erlang:length/1,
                                  maps:values(
                                      dependency_graph_generator:generate_randomly(VertexNum,
                                                                                   EdgeNum))))
            end).

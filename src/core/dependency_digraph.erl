-module(dependency_digraph).

-export([from_dependency_graph/1, is_vertex_splitter/2,
         satisfy_vertex_splitter_constraint1/2, satisfy_vertex_splitter_constraint2/2]).

-export_type([t/0, vertex/0]).

-type t() :: digraph:graph().
-type vertex() :: atom().

%% ===================================================================
%% Public API
%% ===================================================================
-spec from_dependency_graph(dependency_graph:t()) -> t().
from_dependency_graph(DependencyGraph) ->
    Vertices = maps:keys(DependencyGraph),
    Edges =
        [{From, To} || From <- maps:keys(DependencyGraph), To <- maps:get(From, DependencyGraph)],
    my_digraph:create(Vertices, Edges).

% NOTE:
% The argument graph is assumed to be a connected DAG.
% To reduce computation time, do not check whether `ConnectedDAG` is a connected DAG.
-spec is_vertex_splitter(t(), [vertex()]) -> boolean().
is_vertex_splitter(ConnectedDAG, Vertices) ->
    satisfy_vertex_splitter_constraint1(ConnectedDAG, Vertices)
    andalso satisfy_vertex_splitter_constraint2(ConnectedDAG, Vertices).

% NOTE:
% The argument graph is assumed to be a connected DAG.
% To reduce computation time, do not check whether `ConnectedDAG` is a connected DAG.
-spec satisfy_vertex_splitter_constraint1(t(), [vertex()]) -> boolean().
satisfy_vertex_splitter_constraint1(ConnectedDAG, Vertices) ->
    lists:sort(
        digraph_utils:reachable(Vertices, ConnectedDAG))
    =:= lists:sort(Vertices).

% NOTE:
% The argument graph is assumed to be a connected DAG.
% To reduce computation time, do not check whether `ConnectedDAG` is a connected DAG.
-spec satisfy_vertex_splitter_constraint2(t(), [vertex()]) -> boolean().
satisfy_vertex_splitter_constraint2(ConnectedDAG, Vertices) ->
    not
        (lists:sort(
             digraph:vertices(ConnectedDAG))
         =/= lists:sort(Vertices))
    orelse length(digraph_utils:components(
                      digraph_utils:subgraph(ConnectedDAG,
                                             digraph:vertices(ConnectedDAG) -- Vertices)))
           >= 2.

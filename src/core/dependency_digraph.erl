-module(dependency_digraph).

-export([from_dependency_graph/1]).

-export_type([t/0, vertex/0]).

-type t() :: digraph:graph().
-type vertex() :: atom().

-spec from_dependency_graph(dependency_graph:t()) -> t().
from_dependency_graph(DependencyGraph) ->
    Vertices = maps:keys(DependencyGraph),
    Edges =
        [{From, To} || From <- maps:keys(DependencyGraph), To <- maps:get(From, DependencyGraph)],
    my_digraph:create(Vertices, Edges).

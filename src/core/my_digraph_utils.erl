-module(my_digraph_utils).

-export([get_strong_component/2, get_cyclic_strong_component/2, clone/1]).

-spec get_strong_component(digraph:graph(), digraph:vertex()) ->
                              [digraph:vertex()] | false.
get_strong_component(Digraph, Vertex) ->
    StrongComponents = digraph_utils:strong_components(Digraph),
    case lists:filter(fun(Component) -> lists:member(Vertex, Component) end, StrongComponents)
    of
        [C] -> C;
        _ -> false
    end.

-spec get_cyclic_strong_component(digraph:graph(), digraph:vertex()) ->
                                     [digraph:vertex()] | false.
get_cyclic_strong_component(Digraph, Vertex) ->
    CyclicStrongComponents = digraph_utils:cyclic_strong_components(Digraph),
    case lists:filter(fun(Component) -> lists:member(Vertex, Component) end,
                      CyclicStrongComponents)
    of
        [C] -> C;
        _ -> false
    end.

-spec clone(digraph:graph()) -> digraph:graph().
clone(Digraph) -> digraph_utils:subgraph(Digraph, digraph:vertices(Digraph)).

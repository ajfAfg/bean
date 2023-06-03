-module(my_digraph_utils).

-export([clone/1]).

-spec clone(digraph:graph()) -> digraph:graph().
clone(Digraph) -> digraph_utils:subgraph(Digraph, digraph:vertices(Digraph)).

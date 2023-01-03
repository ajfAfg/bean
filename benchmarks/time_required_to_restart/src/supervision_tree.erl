-module(supervision_tree).

-export([from_supervisor_specs/1]).

-type sup_spec() :: {supervisor:sup_flags(), [supervisor:child_spec()]}.
% -type supervision_tree() :: {supervisor:strategy(), [supervision_tree() | atom()]}.
-type supervision_tree() :: digraph:graph().
-type vertex() :: atom().
-type leaf() :: atom().
-type vertex_label() ::
    #{strategy => one_for_all | one_for_one | rest_for_one,
      children_order => fun((vertex() | leaf()) -> integer())}.

-spec from_supervisor_specs([{atom(), sup_spec()}]) -> supervision_tree().
from_supervisor_specs(Sups) ->
    Tree = digraph:new(),
    lists:foreach(fun({SupName, {SupFlags, ChildSpecs}}) ->
                     ChildNames =
                         lists:map(fun(ChildSpec) -> maps:get(id, ChildSpec) end, ChildSpecs),
                     Label =
                         #{strategy => maps:get(strategy, SupFlags),
                           children_order =>
                               fun(V) ->
                                  maps:get(V,
                                           maps:from_list(
                                               lists:map(fun({Index, Name}) -> {Name, Index} end,
                                                         lists:enumerate(ChildNames))))
                               end},
                     digraph:add_vertex(Tree, SupName, Label),

                     lists:foreach(fun(ChildName) ->
                                      case digraph:vertex(Tree, ChildName) of
                                          {ChildName, _} -> ok;
                                          false -> digraph:add_vertex(Tree, ChildName)
                                      end,
                                      digraph:add_edge(Tree, SupName, ChildName)
                                   end,
                                   ChildNames)
                  end,
                  Sups),
    Tree.

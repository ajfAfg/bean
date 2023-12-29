-module(supervision_tree).

-export([from_supervisor_specs/1, calc_cost/1]).

-export_type([supervision_tree/0]).

-type supervision_tree() :: digraph:graph().
-type sup_spec() :: {supervisor:sup_flags(), [supervisor:child_spec()]}.

% NOTE: The types of a vertex, a leaf, and a vertex label.
% -type vertex() :: atom().
% -type leaf() :: atom().
% -type vertex_label() ::
%     #{strategy => one_for_all | one_for_one | rest_for_one,
%       children_order => fun((vertex() | leaf()) -> integer())}.

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

-spec calc_cost(supervision_tree()) -> non_neg_integer().
calc_cost(SupervisionTree) ->
    GenServerNames =
        lists:filter(fun(V) -> digraph:out_degree(SupervisionTree, V) =:= 0 end,
                     digraph:vertices(SupervisionTree)),
    lists:sum(
        lists:map(fun(GenServerName) -> calc_cost(SupervisionTree, GenServerName) end,
                  GenServerNames)).

calc_cost(SupervisionTree, GenServerName) ->
    Parent = hd(digraph:in_neighbours(SupervisionTree, GenServerName)),
    {Parent, #{strategy := Strategy, children_order := ChildrenOrder}} =
        digraph:vertex(SupervisionTree, Parent),
    Names =
        case Strategy of
            one_for_one -> [GenServerName];
            one_for_all -> digraph:out_neighbours(SupervisionTree, Parent);
            rest_for_one ->
                lists:filter(fun(Name) -> ChildrenOrder(GenServerName) =< ChildrenOrder(Name) end,
                             digraph:out_neighbours(SupervisionTree, Parent))
        end,
    lists:sum(
        lists:map(fun(Name) -> calc_cost_(SupervisionTree, Name) end, Names)).

calc_cost_(SupervisionTree, Name) ->
    case digraph:out_neighbours(SupervisionTree, Name) of
        [] -> 1;
        Children ->
            lists:sum(
                lists:map(fun(Child) -> calc_cost_(SupervisionTree, Child) end, Children))
    end.

-module(supervision_tree).

-export([are_isomorphic/2, calc_cost/1]).

-export_type([t/0, child/0]).

-type t() :: {supervisor:strategy(), [child()]}.
-type child() :: behavior:name() | t().

-spec are_isomorphic(t(), t()) -> boolean().
are_isomorphic({_, _} = Tree1, {_, _} = Tree2) -> are_isomorphic_(Tree1, Tree2).

-spec are_isomorphic_(child(), child()) -> boolean().
are_isomorphic_(Child, Child) -> true;
are_isomorphic_({Strategy, Children1}, {Strategy, Children2}) ->
    {NewChildren1, NewChildren2} =
        case Strategy =:= rest_for_one of
            true -> {Children1, Children2};
            false -> {lists:sort(Children1), lists:sort(Children2)}
        end,
    length(Children1) =:= length(Children2)
    andalso lists:all(fun({Child1, Child2}) -> are_isomorphic_(Child1, Child2) end,
                      lists:zip(NewChildren1, NewChildren2));
are_isomorphic_(_, _) -> false.

-spec calc_cost(t()) -> non_neg_integer().
calc_cost({one_for_all, Children} = Tree) ->
    lists:sum([case is_atom(Child) of
                   true -> length(take_behavior_names(Tree));
                   false -> calc_cost(Child)
               end
               || Child <- Children]);
calc_cost({one_for_one, Children}) ->
    lists:sum([case is_atom(Child) of
                   true -> 1;
                   false -> calc_cost(Child)
               end
               || Child <- Children]);
calc_cost({rest_for_one, Children}) ->
    foldl_with_rest(fun(Head, Acc, Rest) ->
                       case is_atom(Head) of
                           true ->
                               length(lists:flatten(
                                          lists:map(fun take_behavior_names/1, [Head | Rest])));
                           false -> calc_cost(Head)
                       end
                       + Acc
                    end,
                    0,
                    Children);
calc_cost({simple_one_for_one, _}) -> throw(not_supported).

-spec take_behavior_names(child()) -> [behavior:name()].
take_behavior_names({_, Children}) ->
    lists:flatten(
        lists:map(fun take_behavior_names/1, Children));
take_behavior_names(Name) -> Name.

-spec foldl_with_rest(Fun, Acc0, List) -> Acc1
    when Fun :: fun((Elem :: T, AccIn, Rest :: List) -> AccOut),
         Acc0 :: term(),
         Acc1 :: term(),
         AccIn :: term(),
         AccOut :: term(),
         List :: [T],
         T :: term().
foldl_with_rest(_Fun, Acc0, []) -> Acc0;
foldl_with_rest(Fun, Acc0, [Head | Rest]) ->
    foldl_with_rest(Fun, Fun(Head, Acc0, Rest), Rest).

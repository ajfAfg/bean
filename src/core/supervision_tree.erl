-module(supervision_tree).

-export([are_isomorphic/2]).

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

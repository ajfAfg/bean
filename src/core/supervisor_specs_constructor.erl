-module(supervisor_specs_constructor).

-export([construct/1]).

-spec construct(supervision_tree:t()) -> [supervisor_spec:t()].
construct(SupervisionTree) ->
    convert_sup_specs_from_grouped_dependencies(SupervisionTree,
                                                create_sup_names(SupervisionTree),
                                                []).

-spec convert_sup_specs_from_grouped_dependencies(supervision_tree:child(),
                                                  sup_names(),
                                                  [supervisor_spec:t()]) ->
                                                     [supervisor_spec:t()].
convert_sup_specs_from_grouped_dependencies(Name, _, Acc) when is_atom(Name) -> Acc;
convert_sup_specs_from_grouped_dependencies({Strategy, Children} = SupervisionTree,
                                            Names,
                                            Acc) ->
    SupSpec =
        #{name => maps:get(SupervisionTree, Names),
          sup_flags =>
              #{strategy => Strategy,
                intensity => 1,
                period => 5},
          child_specs => lists:map(fun(Child) -> create_child_spec(Child, Names) end, Children)},
    lists:foldl(fun(Child, A) -> convert_sup_specs_from_grouped_dependencies(Child, Names, A)
                end,
                [SupSpec | Acc],
                Children).

-type sup_names() :: #{supervision_tree:t() => atom()}.

-spec create_sup_names(supervision_tree:t()) -> sup_names().
create_sup_names({_, Children} = SupervisionTree) ->
    % NOTE: The name of root in a supervision tree is `bean`.
    lists:foldl(fun maps:merge/2,
                #{SupervisionTree => bean},
                lists:map(fun create_sup_names_aux/1, Children)).

create_sup_names_aux(Name) when is_atom(Name) -> #{};
create_sup_names_aux({_, Children} = SupervisionTree) ->
    lists:foldl(fun maps:merge/2,
                #{SupervisionTree => make_name()},
                lists:map(fun create_sup_names_aux/1, Children)).

-spec make_name() -> atom().
make_name() ->
    erlang:list_to_atom(
        erlang:ref_to_list(make_ref())).

-spec create_child_spec(supervision_tree:child(), sup_names()) -> supervisor:child_spec().
create_child_spec(Name, _) when is_atom(Name) ->
    #{id => Name,
      start => {gen_server, start_link, [{local, Name}, Name, [], []]},
      type => worker};
create_child_spec(Child, Names) ->
    Name = maps:get(Child, Names),
    #{id => Name,
      start => {supervisor, start_link, [Name, []]},
      type => supervisor}.

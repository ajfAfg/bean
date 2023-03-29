-module(supervision_tree_constructor).

-export([construct/1]).

-export_type([sup_spec/0]).

-record(sup_spec,
        {name :: atom(),
         sup_flags :: supervisor:sup_flags(),
         child_specs :: [supervisor:child_spec()]}).

-type sup_spec() :: #sup_spec{}.

-spec construct([cerl:c_module()]) -> [sup_spec()].
construct(CModules) ->
    GroupedDependencies =
        optimum_supervision_tree_solver:solve(
            gen_server_dependencies:extract_dependencies(CModules)),
    convert_sup_specs_from_grouped_dependencies(GroupedDependencies,
                                                create_sup_names(GroupedDependencies),
                                                []).

-spec convert_sup_specs_from_grouped_dependencies(supervision_tree:t(),
                                                  sup_names(),
                                                  [sup_spec()]) ->
                                                     [sup_spec()].
convert_sup_specs_from_grouped_dependencies(GenServer, _, Acc) when is_atom(GenServer) ->
    Acc;
convert_sup_specs_from_grouped_dependencies({Strategy, Deps2} = Deps1, Names, Acc) ->
    SupSpec =
        #sup_spec{name = maps:get(Deps1, Names),
                  sup_flags =
                      #{strategy => Strategy,
                        % TODO: Give the debug information only when running the benchmark to measure the time to restart gen_servers
                        intensity => 1000,
                        period => 1},
                  child_specs = lists:map(fun(Dep) -> create_child_spec(Dep, Names) end, Deps2)},
    lists:foldl(fun(Dep, A) -> convert_sup_specs_from_grouped_dependencies(Dep, Names, A) end,
                [SupSpec | Acc],
                Deps2).

-type sup_names() :: #{supervision_tree:t() => atom()}.

-spec create_sup_names(supervision_tree:t()) -> sup_names().
create_sup_names({_, Deps2} = Deps1) ->
    % NOTE: The name of root in a supervision tree is `bean`.
    lists:foldl(fun maps:merge/2,
                #{Deps1 => bean},
                lists:map(fun create_sup_names_aux/1, Deps2)).

create_sup_names_aux(GenServer) when is_atom(GenServer) -> #{};
create_sup_names_aux({_, Deps2} = Deps1) ->
    lists:foldl(fun maps:merge/2,
                #{Deps1 => make_name()},
                lists:map(fun create_sup_names_aux/1, Deps2)).

-spec make_name() -> atom().
make_name() ->
    erlang:list_to_atom(
        erlang:ref_to_list(make_ref())).

-spec create_child_spec(supervision_tree:child(), sup_names()) -> supervisor:child_spec().
create_child_spec(GenServer, _) when is_atom(GenServer) ->
    #{id => GenServer,
      start =>
          % TODO: Give the debug information only when running the benchmark to measure the time to restart gen_servers
          {gen_server, start_link, [{local, GenServer}, GenServer, [], [{debug, [statistics]}]]},
      type => worker};
create_child_spec(Dep, Names) ->
    Name = maps:get(Dep, Names),
    #{id => Name,
      start => {supervisor, start_link, [Name, []]},
      type => supervisor}.

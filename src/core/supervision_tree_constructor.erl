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
        gen_server_dependencies:group(
            gen_server_dependencies:extract_dependencies(CModules)),
    convert_sup_specs_from_grouped_dependencies(GroupedDependencies,
                                                create_sup_names(GroupedDependencies),
                                                []).

-spec
    convert_sup_specs_from_grouped_dependencies(gen_server_dependencies:grouped_dependencies(),
                                                sup_names(),
                                                [sup_spec()]) ->
                                                   [sup_spec()].
convert_sup_specs_from_grouped_dependencies(GenServer, _, Acc) when is_atom(GenServer) ->
    Acc;
convert_sup_specs_from_grouped_dependencies({Strategy, Deps2} = Deps1, Names, Acc) ->
    SupSpec =
        #sup_spec{name = maps:get(Deps1, Names),
                  sup_flags = #{strategy => Strategy},
                  child_specs = lists:map(fun(Dep) -> create_child_spec(Dep, Names) end, Deps2)},
    lists:foldl(fun(Dep, A) -> convert_sup_specs_from_grouped_dependencies(Dep, Names, A) end,
                [SupSpec | Acc],
                Deps2).

-type sup_names() :: #{gen_server_dependencies:grouped_dependencies() => atom()}.

-spec create_sup_names(gen_server_dependencies:grouped_dependencies()) -> sup_names().
create_sup_names(GenServer) when is_atom(GenServer) -> #{};
create_sup_names({_, Deps2} = Deps1) ->
    lists:foldl(fun maps:merge/2,
                #{Deps1 => make_name()},
                lists:map(fun create_sup_names/1, Deps2)).

-spec make_name() -> atom().
make_name() ->
    erlang:list_to_atom(
        erlang:ref_to_list(make_ref())).

-spec create_child_spec(gen_server_dependencies:grouped_dependencies() | atom(),
                        sup_names()) ->
                           supervisor:child_spec().
create_child_spec(GenServer, _) when is_atom(GenServer) ->
    #{id => GenServer,
      start => {gen_server, start_link, [{local, GenServer}, GenServer, [], []]},
      type => worker};
create_child_spec(Dep, Names) ->
    Name = maps:get(Dep, Names),
    #{id => Name,
      start => {supervisor, start_link, [Name, []]},
      type => supervisor}.

-module(supervision_tree_constructor).

-export([construct/1]).

-spec construct([cerl:c_module()]) -> {atom(), [supervisor:child_spec()]}.
construct(CModules) ->
    Names = get_gen_server_module_names(CModules),
    ChildSpecs = lists:map(fun construct_child_spec/1, Names),
    construct_supervisor(ChildSpecs).

get_gen_server_module_names(CModules) ->
    F = fun(CModule) ->
           case c_gen_server:is_gen_server(CModule) of
               true ->
                   cerl:atom_val(
                       cerl:module_name(CModule));
               false -> nil
           end
        end,
    lists:filter(fun(E) -> E =/= nil end, lists:map(F, CModules)).

construct_child_spec(ModuleName) ->
    #{id => ModuleName, start => {ModuleName, start_link, []}}.

construct_supervisor(ChildSpecs) -> {bean_sup, ChildSpecs}.

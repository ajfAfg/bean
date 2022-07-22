% TODO:
% In the future, only information to configure supervisors will be generated
% so that both Erlang and Elixir can be supported.
-module(supervision_tree_constructor).

-export([construct/1]).

-spec construct([cerl:c_module()]) -> {atom(), [supervisor:child_spec()]}.
construct(CModules) ->
    Names = get_genserver_module_names(CModules),
    ChildSpecs = lists:map(fun construct_child_spec/1, Names),
    construct_supervisor(ChildSpecs).

get_genserver_module_names([]) -> [];
get_genserver_module_names([CModule | Rest]) ->
    case is_genserver(CModule) of
        true ->
            [cerl:atom_val(
                 cerl:module_name(CModule))
             | get_genserver_module_names(Rest)];
        false -> get_genserver_module_names(Rest)
    end.

% TODO: Support all behaviors in the future
is_genserver(CModule) ->
    Attrs = cerl:module_attrs(CModule),
    Fun = fun({CLiteral1, CLiteral2}) ->
             cerl:atom_val(CLiteral1) =:= behaviour
             andalso lists:member(gen_server, my_cerl:list_val(CLiteral2))
          end,
    lists:any(Fun, Attrs).

construct_child_spec(ModuleName) ->
    #{id => ModuleName, start => {ModuleName, start_link, []}}.

construct_supervisor(ChildSpecs) -> {bean_sup, ChildSpecs}.

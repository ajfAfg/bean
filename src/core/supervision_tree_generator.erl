% TODO:
% In the future, only information to configure supervisors will be generated
% so that both Erlang and Elixir can be supported.
-module(supervision_tree_generator).

-export([generate/1]).

-spec generate([cerl:c_module()]) -> ok.
generate(CModules) ->
    Names = get_genserver_module_names(CModules),
    ChildSpecs = lists:map(fun generate_child_spec/1, Names),
    Sup = generate_supervisor(ChildSpecs),
    rebar_api:console("~s", [Sup]),
    file:write_file("src/bean_sup.erl", Sup).

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

generate_child_spec(ModuleName) ->
    io_lib:format("#{id => ~p, start => {~p, start_link, []}}", [ModuleName, ModuleName]).

generate_supervisor(ChildSpecs) ->
    Format =
        "-module(bean_sup).~n"
        "-behavior(supervisor).~n"
        "-export([start_link/0]).~n"
        "-export([init/1]).~n"
        "~n"
        "start_link() -> supervisor:start_link(bean_sup, []).~n"
        "init(_Args) ->"
        "SupFlags = #{},"
        "ChildSpecs = [~s],"
        "{ok, {SupFlags, ChildSpecs}}.~n",
    io_lib:format(Format, [lists:join(",", ChildSpecs)]).

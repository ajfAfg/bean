% TODO:
% To increase the accuracy of the dependencies to be extracted,
% symbolic execution should be performed.

-module(gen_server_dependencies).

-export([extract_dependencies/1, group/1]).

-export_type([dependencies/0, grouped_dependencies/0]).

-type dependencies() :: #{atom() => [atom()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% extract_dependencies/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(fun_signature, {name :: atom(), arity :: non_neg_integer()}).
-record(fun_def, {fun_signature :: #fun_signature{}, body :: cerl:cerl()}).

-spec extract_dependencies([cerl:c_module()]) -> dependencies().
extract_dependencies([]) -> #{};
extract_dependencies([CModule | CModules]) ->
    Dependency =
        case c_gen_server:is_gen_server(CModule) of
            true ->
                ModuleName =
                    cerl:atom_val(
                        cerl:module_name(CModule)),
                FunDefs =
                    remove_functions_other_than_gen_server_callback_functions(take_functions(CModule)),
                Bodies = lists:map(fun(#fun_def{body = Body}) -> Body end, FunDefs),
                #{ModuleName => lists:foldl(fun extract_dependencies_from_fun_body/2, [], Bodies)};
            false -> #{}
        end,
    maps:merge(Dependency, extract_dependencies(CModules)).

-spec take_functions(cerl:c_module()) -> [#fun_def{}].
take_functions(CModule) ->
    FunctionDefs = cerl:module_defs(CModule),
    Fun = fun({CVar, CFun}) ->
             Sig = #fun_signature{name = cerl:fname_id(CVar), arity = cerl:fname_arity(CVar)},
             #fun_def{fun_signature = Sig, body = cerl:fun_body(CFun)}
          end,
    lists:map(Fun, FunctionDefs).

-spec remove_functions_other_than_gen_server_callback_functions([#fun_def{}]) ->
                                                                   [#fun_def{}].
remove_functions_other_than_gen_server_callback_functions(FunDefs) ->
    Pred =
        fun(#fun_def{fun_signature = Sig}) ->
           lists:member(Sig, get_gen_server_callback_functions())
        end,
    lists:filter(Pred, FunDefs).

-spec get_gen_server_callback_functions() -> [#fun_signature{}].
get_gen_server_callback_functions() ->
    [#fun_signature{name = code_change, arity = 3},
     #fun_signature{name = format_status, arity = 1},
     #fun_signature{name = format_status, arity = 2},
     #fun_signature{name = handle_call, arity = 3},
     #fun_signature{name = handle_cast, arity = 2},
     #fun_signature{name = handle_continue, arity = 2},
     #fun_signature{name = handle_info, arity = 2},
     #fun_signature{name = init, arity = 1},
     #fun_signature{name = terminate, arity = 2}].

-spec extract_dependencies_from_fun_body(cerl:cerl(), [atom()]) -> [atom()].
extract_dependencies_from_fun_body({c_alias, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_apply, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_binary, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_bitstr, _, _, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_call, _, Module, Name, Args}, Acc) ->
    % TODO: Assume that `Module`, `Name` and the first element of `Args` are `c_literal()` type
    % TODO: Support `gen_server:stop/1` and so on
    Fun = fun ({c_literal, _, gen_server},
               {c_literal, _, call},
               [{c_literal, _, GenServerName} | _]) ->
                  {some, GenServerName};
              ({c_literal, _, gen_server},
               {c_literal, _, cast},
               [{c_literal, _, GenServerName} | _]) ->
                  {some, GenServerName};
              (_, _, _) -> none
          end,
    case Fun(Module, Name, Args) of
        {some, GenServerName} -> [GenServerName | Acc];
        none -> Acc
    end;
extract_dependencies_from_fun_body({c_case, _, _, Clauses}, Acc) ->
    lists:foldl(fun(Clause, A) -> extract_dependencies_from_fun_body(Clause, A) end,
                Acc,
                Clauses);
extract_dependencies_from_fun_body({c_catch, _, Body}, Acc) ->
    extract_dependencies_from_fun_body(Body, Acc);
extract_dependencies_from_fun_body({c_clause, _, _, _, Body}, Acc) ->
    extract_dependencies_from_fun_body(Body, Acc);
extract_dependencies_from_fun_body({c_cons, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_fun, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_let, _, _, Arg, Body}, Acc) ->
    Acc2 = extract_dependencies_from_fun_body(Arg, Acc),
    extract_dependencies_from_fun_body(Body, Acc2);
extract_dependencies_from_fun_body({c_letrec, _, _, _}, Acc) ->
    Acc; % TODO: Support
% extract_dependencies_from_fun_body({c_literal, _, _}, ModuleName, Acc) ->
%     Acc; % TODO: Support
extract_dependencies_from_fun_body({c_map, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_map_pair, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_module, _, _, _, _, _}, Acc) ->
    Acc; % NOTE: Probably no match
extract_dependencies_from_fun_body({c_primop, _, _, _}, Acc) ->
    Acc; % NOTE: Ignore
extract_dependencies_from_fun_body({c_receive, _, Clauses, _, _}, Acc) ->
    lists:foldl(fun(Clause, A) -> extract_dependencies_from_fun_body(Clause, A) end,
                Acc,
                Clauses);
extract_dependencies_from_fun_body({c_seq, _, Arg, Body}, Acc) ->
    Acc2 = extract_dependencies_from_fun_body(Arg, Acc),
    extract_dependencies_from_fun_body(Body, Acc2);
extract_dependencies_from_fun_body({c_try, _, _, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_tuple, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_values, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body({c_var, _, _}, Acc) ->
    Acc; % TODO: Support
extract_dependencies_from_fun_body(_, Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% group/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: Export this type?
-type grouped_dependencies() ::
    {supervisor:strategy(), [grouped_dependencies() | atom()]}.

-spec group(dependencies()) -> grouped_dependencies().
group(Dependencies) ->
    Graph = new_digraph_from_dependencies(Dependencies),
    group_cyclic_strongly_connected_components(Graph),
    keep_grouping_until_vertices_num_reaches_one(Graph),
    case digraph:no_vertices(Graph) of
        1 -> hd(digraph:vertices(Graph));
        _ -> {one_for_one, digraph:vertices(Graph)}
    end.

-spec new_digraph_from_dependencies(dependencies()) -> digraph:graph().
new_digraph_from_dependencies(Dependencies) ->
    G = digraph:new(),
    lists:foreach(fun(K) -> digraph:add_vertex(G, K) end, maps:keys(Dependencies)),
    maps:foreach(fun(K, V) -> lists:foreach(fun(E) -> digraph:add_edge(G, K, E) end, V) end,
                 Dependencies),
    G.

-spec keep_grouping_until_vertices_num_reaches_one(digraph:graph()) -> ok.
keep_grouping_until_vertices_num_reaches_one(Graph) ->
    case digraph:no_edges(Graph) of
        0 -> ok;
        _ ->
            group_longest_straight_path(Graph),
            group_many_neighbours(Graph),
            keep_grouping_until_vertices_num_reaches_one(Graph)
    end.

-spec group_cyclic_strongly_connected_components(digraph:graph()) -> ok.
group_cyclic_strongly_connected_components(Graph) ->
    lists:foreach(fun(Vertices) -> my_digraph:union(Graph, Vertices, {one_for_all, Vertices})
                  end,
                  digraph_utils:cyclic_strong_components(Graph)).

% TODO: Dirty code
-spec group_longest_straight_path(digraph:graph()) -> ok.
group_longest_straight_path(Graph) ->
    OptionVertex =
        case lists:filter(fun(V) -> digraph:in_degree(Graph, V) =:= 0 end,
                          digraph:vertices(Graph))
        of
            [] ->
                case lists:filter(fun(V) -> digraph:in_degree(Graph, V) =:= 1 end,
                                  digraph:vertices(Graph))
                of
                    [] -> none;
                    [Vertex | _] -> {some, Vertex}
                end;
            [Vertex | _] -> {some, Vertex}
        end,
    case OptionVertex of
        none -> ok;
        {some, V} ->
            case my_digraph:get_longest_straight_path(Graph, V) of
                false -> ok;
                Path ->
                    my_digraph:union(Graph, Path, {rest_for_one, Path}),
                    group_longest_straight_path(Graph)
            end
    end.

% TODO: Dirty code
% TODO: Worst function name
-spec group_many_neighbours(digraph:graph()) -> ok.
group_many_neighbours(Graph) ->
    Pred1 =
        fun(V1) ->
           digraph:in_degree(Graph, V1) >= 2
           andalso lists:all(fun(V2) -> digraph:in_degree(Graph, V2) =:= 0 end,
                             digraph:in_neighbours(Graph, V1))
        end,
    Pred2 =
        fun(V1) ->
           digraph:out_degree(Graph, V1) >= 2
           andalso lists:all(fun(V2) -> digraph:out_degree(Graph, V2) =:= 0 end,
                             digraph:out_neighbours(Graph, V1))
        end,
    OptionVerticesList =
        [case lists:filter(Pred1, digraph:vertices(Graph)) of
             [] -> none;
             [Vertex | _] -> {some, digraph:in_neighbours(Graph, Vertex)}
         end,
         case lists:filter(Pred2, digraph:vertices(Graph)) of
             [] -> none;
             [Vertex | _] -> {some, digraph:out_neighbours(Graph, Vertex)}
         end],
    case OptionVerticesList of
        [none, none] -> ok;
        _ ->
            VerticesList =
                lists:map(fun({some, Vs}) -> Vs end,
                          lists:filter(fun (none) -> false;
                                           ({some, _}) -> true
                                       end,
                                       OptionVerticesList)),
            lists:foreach(fun(Vs) -> my_digraph:union(Graph, Vs, {one_for_one, Vs}) end,
                          VerticesList),
            group_many_neighbours(Graph)
    end.

% TODO:
% To increase the accuracy of the dependencies to be extracted,
% symbolic execution should be performed.

-module(dependency_extractor).

-export([extract/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% extract/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(fun_signature, {name :: atom(), arity :: non_neg_integer()}).
-record(fun_def, {fun_signature :: #fun_signature{}, body :: cerl:cerl()}).

-spec extract([cerl:c_module()]) -> option:t(dependency_graph:t()).
extract(CModules) ->
    G = extract_(CModules),
    case map_size(G) =:= 0 of
        true -> none;
        false -> {some, inverse_dependencies(G)}
    end.

-spec extract_([cerl:c_module()]) -> #{behavior:name() => [behavior:name()]}.
extract_([]) -> #{};
extract_([CModule | CModules]) ->
    Dependency =
        case c_gen_server:is_gen_server(CModule) of
            true ->
                ModuleName =
                    cerl:atom_val(
                        cerl:module_name(CModule)),
                FunDefs =
                    remove_functions_other_than_gen_server_callback_functions(take_functions(CModule)),
                Bodies = lists:map(fun(#fun_def{body = Body}) -> Body end, FunDefs),
                #{ModuleName => lists:foldl(fun extract_from_fun_body/2, [], Bodies)};
            false -> #{}
        end,
    maps:merge(Dependency, extract_(CModules)).

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

-spec extract_from_fun_body(cerl:cerl(), [atom()]) -> [atom()].
extract_from_fun_body({c_alias, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_apply, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_binary, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_bitstr, _, _, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_call, _, Module, Name, Args}, Acc) ->
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
extract_from_fun_body({c_case, _, _, Clauses}, Acc) ->
    lists:foldl(fun(Clause, A) -> extract_from_fun_body(Clause, A) end, Acc, Clauses);
extract_from_fun_body({c_catch, _, Body}, Acc) -> extract_from_fun_body(Body, Acc);
extract_from_fun_body({c_clause, _, _, _, Body}, Acc) -> extract_from_fun_body(Body, Acc);
extract_from_fun_body({c_cons, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_fun, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_let, _, _, Arg, Body}, Acc) ->
    Acc2 = extract_from_fun_body(Arg, Acc),
    extract_from_fun_body(Body, Acc2);
extract_from_fun_body({c_letrec, _, _, _}, Acc) ->
    Acc; % TODO: Support
% extract_from_fun_body({c_literal, _, _}, ModuleName, Acc) ->
%     Acc; % TODO: Support
extract_from_fun_body({c_map, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_map_pair, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_module, _, _, _, _, _}, Acc) ->
    Acc; % NOTE: Probably no match
extract_from_fun_body({c_primop, _, _, _}, Acc) ->
    Acc; % NOTE: Ignore
extract_from_fun_body({c_receive, _, Clauses, _, _}, Acc) ->
    lists:foldl(fun(Clause, A) -> extract_from_fun_body(Clause, A) end, Acc, Clauses);
extract_from_fun_body({c_seq, _, Arg, Body}, Acc) ->
    Acc2 = extract_from_fun_body(Arg, Acc),
    extract_from_fun_body(Body, Acc2);
extract_from_fun_body({c_try, _, _, _, _, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_tuple, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_values, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body({c_var, _, _}, Acc) ->
    Acc; % TODO: Support
extract_from_fun_body(_, Acc) -> Acc.

-spec inverse_dependencies(#{behavior:name() => [behavior:name()]}) ->
                              #{behavior:name() => [behavior:name()]}.
inverse_dependencies(Dependencies) ->
    maps:fold(fun(Key, Value, NewDependencies) ->
                 lists:foldl(fun(E, Acc) -> maps:update_with(E, fun(List) -> [Key | List] end, Acc)
                             end,
                             NewDependencies,
                             Value)
              end,
              maps:from_keys(
                  maps:keys(Dependencies), []),
              Dependencies).

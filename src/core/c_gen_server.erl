-module(c_gen_server).

-export([is_gen_server/1]).

-spec is_gen_server(cerl:c_module()) -> boolean().
is_gen_server(CModule) ->
    Attrs = cerl:module_attrs(CModule),
    Fun = fun({CLiteral1, CLiteral2}) ->
             cerl:atom_val(CLiteral1) =:= behaviour
             andalso lists:member(gen_server, my_cerl:list_val(CLiteral2))
          end,
    lists:any(Fun, Attrs).

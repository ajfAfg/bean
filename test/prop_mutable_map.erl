-module(prop_mutable_map).

-compile(export_all).

-import(proper_helper, [random_type/0]).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% mutable_map:put/3 and mutable_map:find/2 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_put_and_find1(doc) -> "The last put term can be found".

prop_put_and_find1() ->
    ?FORALL({Key, Value1, Value2},
            tuple([random_type(), random_type(), random_type()]),
            begin
                MutableMap = mutable_map:new(),
                mutable_map:put(Key, Value1, MutableMap),
                mutable_map:put(Key, Value2, MutableMap),
                {ok, Value2} =:= mutable_map:find(Key, MutableMap)
            end).

prop_put_and_find2(doc) -> "A term that is not put cannot be found".

prop_put_and_find2() ->
    ?FORALL(Key,
            random_type(),
            begin
                MutableMap = mutable_map:new(),
                error =:= mutable_map:find(Key, MutableMap)
            end).

-module(my_cerl).

-export([list_val/1]).

-spec list_val(cerl:c_literal()) -> list().
list_val({c_literal, _, Val}) -> Val.

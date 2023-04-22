-module(proper_helper).

-export([random_type/0]).

-include_lib("proper/include/proper.hrl").

random_type() ->
    hd(my_lists:shuffle([atom(), binary(), boolean(), float(), integer(), string()])).

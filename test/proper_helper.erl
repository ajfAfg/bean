-module(proper_helper).

-export([limited_atom/0, limited_atom/1, random_type/0]).

-include_lib("proper/include/proper.hrl").

% NOTE:
% Unlike `atom()`, this function limits the number of atoms generated.
% The default limit on the number of atoms is 1048576,
% so if `atom()` is used, the number of atoms may become too large
% when the number of tests increases, causing the Erlang VM to crash.
% This function is mainly used to avoid this problem.
-spec limited_atom() -> proper_types:type().
limited_atom() -> limited_atom(1000).

-spec limited_atom(integer()) -> proper_types:type().
limited_atom(N) -> oneof([list_to_atom(integer_to_list(X)) || X <- lists:seq(1, N)]).

% NOTE: The policy is to not use the usual atom generator.
-spec random_type() -> proper_types:type().
random_type() ->
    oneof([limited_atom(), binary(), boolean(), float(), integer(), string()]).

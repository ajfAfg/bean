% NOTE: API resembles the module `maps`.
-module(mutable_map).

-export([new/0, put/3, find/2]).

-export_type([t/0, key/0, value/0]).

-opaque t() :: ets:table().

-type key() :: term().
-type value() :: term().

-spec new() -> t().
new() -> ets:new(?MODULE, [set, private]).

-spec put(key(), value(), t()) -> true.
put(Key, Value, MutableMap) -> ets:insert(MutableMap, {Key, Value}).

-spec find(key(), t()) -> {ok, value()} | error.
find(Key, MutableMap) ->
    case ets:lookup(MutableMap, Key) of
        [{Key, Value}] -> {ok, Value};
        [] -> error
    end.

-module(option).

-export([get/1]).

-export_type([t/1]).

-type t(T) :: {some, T} | none.

% NOTE: No support for `none`
-spec get(t(T)) -> T when T :: term().
get({some, V}) -> V.

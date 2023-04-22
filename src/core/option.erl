-module(option).

-export_type([t/1]).

-type t(T) :: {some, T} | none.

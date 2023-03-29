-module(supervision_tree).

-export_type([t/0, child/0]).

-type t() :: {supervisor:strategy(), [child()]}.
-type child() :: behavior:name() | t().

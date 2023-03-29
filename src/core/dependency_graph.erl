-module(dependency_graph).

-export_type([t/0]).

-type vertex() :: behavior:name().
-type t() :: #{vertex() => [vertex()]}.

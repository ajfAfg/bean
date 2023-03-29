-module(supervisor_spec).

-export_type([t/0]).

-type t() ::
    #{name := behavior:name(),
      sup_flags := supervisor:sup_flags(),
      child_specs := [supervisor:child_spec()]}.

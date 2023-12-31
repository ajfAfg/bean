#!/bin/sh

# NOTE:
# A program to make the rebar application analyzable by Dialyzer.
# The Rebar3 feature [Overrides](https://rebar3.org/docs/configuration/configuration/#overrides)
# could not override the `erl_opts` of the rebar application,
# so the source code is directly rewritten to add `debug_info`.
#
# The reason why this program is not written in `rebar.config` as a one-liner
# is that the Rebar3 feature [Hooks](https://rebar3.org/docs/configuration/configuration/#hooks)
# allows to execute only one command.
# [Hooks uses `open_port/2` to execute a shell command](https://github.com/erlang/rebar3/blob/8f9c87280c5d7b6a6bd4546607a7bb00edc92951/apps/rebar/src/rebar_utils.erl#L196),
# this function does not seem to support multiple command execution.

dir='_build/default/lib/rebar/apps/rebar'

grep 'no_debug_info' "${dir}/rebar.config" >/dev/null 2>&1
if [ $? = 0 ]; then
    case "$(uname -s)" in
    'Darwin') sed -i '' 's/no_debug_info/debug_info/g' "${dir}/rebar.config" ;;
    'Linux') sed -i 's/no_debug_info/debug_info/g' "${dir}/rebar.config" ;;
    *)
        echo "'$(uname -s)' is not supported." 1>&2
        exit 1
        ;;
    esac
    rm -rf "${dir}/ebin"
    rebar3 compile -d
fi

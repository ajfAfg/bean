#!/bin/sh

for i in $(seq 1 12); do
    for j in $(seq 1 10); do
        rebar3 escriptize >/dev/null 2>&1
        _build/default/bin/time_required_to_restart generate-gen-server $i $j

        rebar3 bean >/dev/null 2>&1

        rebar3 escriptize >/dev/null 2>&1
        _build/default/bin/time_required_to_restart measure $i $j

        rm -rf src/bean
        rm -rf src/gen_servers
    done
done

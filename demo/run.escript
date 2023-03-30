#!/usr/bin/env escript
main(_) ->
    supervisor:start_link(bean, []),
    lists:foreach(fun(P) ->
                     timer:sleep(100),
                     io:format("Result: ~p~n", [catch P()])
                  end,
                  procedures()),
    halt().

procedures() ->
    [fun() -> fib_server:calc(10) end,
     fun() -> fib_server:calc(-1) end,
     fun() -> fib_server:calc(10) end].

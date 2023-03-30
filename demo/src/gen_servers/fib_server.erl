-module(fib_server).

-behaviour(gen_server).

-export([calc/1]).
-export([init/1, handle_call/3, handle_cast/2]).

calc(N) -> gen_server:call(?MODULE, {fib, N}, 20000).

init([]) ->
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({fib, K}, _From, N) -> {reply, fib(K), N + 1}.

handle_cast(_Msg, N) -> {noreply, N}.

fib(N) when N < 0 -> throw(invalid_number);
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

-module(fizzbuzz_server).

-behaviour(gen_server).

-export([calc/1]).
-export([init/1, handle_call/3, handle_cast/2]).

calc(N) -> gen_server:call(?MODULE, {fizzbuzz, N}, 20000).

init([]) ->
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({fizzbuzz, K}, _From, N) -> {reply, fizzbuzz(K), N + 1}.

handle_cast(_Msg, N) -> {noreply, N}.

fizzbuzz(N) when N rem 15 =:= 0 -> "FizzBuzz";
fizzbuzz(N) when N rem 3 =:= 0 -> "Fizz";
fizzbuzz(N) when N rem 5 =:= 0 -> "Buzz";
fizzbuzz(N) when is_integer(N) -> N.

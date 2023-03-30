-module(third_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init([]) ->
    io:format("~p starting~n", [?MODULE]),
    {ok, #{}}.

handle_call(_Request, _From, State) -> {reply, first, State}.

handle_cast(_Request, State) -> {noreply, State}.

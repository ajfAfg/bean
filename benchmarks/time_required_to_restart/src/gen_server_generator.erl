-module(gen_server_generator).

-export([generate/2]).

-spec generate(atom(), [atom()]) -> string().
generate(Name, DependencyNames) ->
    GenServerCalls =
        lists:map(fun(N) -> io_lib:format("gen_server:call(~p, Request)", [N]) end,
                  DependencyNames),
    io_lib:format(format(),
                  [Name,
                   % NOTE: No comma when the length of `GenServerCalls` is 0.
                   lists:foldl(fun(Call, Acc) -> Acc ++ Call ++ "," end, "", GenServerCalls)]).

format() ->
    "-module(~p).~n"
    "-behaviour(gen_server).~n"
    "-export([init/1, handle_call/3, handle_cast/2]).~n"
    "init([]) -> {ok, #{}}.~n"
    "handle_call(Request, _From, State) ->"
    "~s"
    "{reply, first, State}.~n"
    "handle_cast(_Request, State) -> {noreply, State}.~n".

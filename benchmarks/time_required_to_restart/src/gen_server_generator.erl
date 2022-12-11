-module(gen_server_generator).

-export([generate/3]).

-spec generate(atom(), [atom()], pos_integer()) -> string().
generate(Name, DependencyNames, DelayTime) ->
    GenServerCalls =
        lists:map(fun(N) -> io_lib:format("gen_server:call(~p, Request)", [N]) end,
                  DependencyNames),
    io_lib:format(format(),
                  [Name,
                   DelayTime,
                   % NOTE: No comma when the length of `GenServerCalls` is 0.
                   lists:foldl(fun(Call, Acc) -> Acc ++ Call ++ "," end, "", GenServerCalls),
                   DelayTime]).

format() ->
    "-module(~p).~n"
    "-behaviour(gen_server).~n"
    "-export([init/1, handle_call/3, handle_cast/2, terminate/2]).~n"
    "% NOTE: Send a message to the measure process. ~n"
    "init([]) -> io:format(?MODULE), timer:sleep(~p),measurer ! ok,{ok, #{}}.~n"
    "handle_call(Request, _From, State) ->"
    "~s"
    "{reply, first, State}.~n"
    "handle_cast(_Request, State) -> {noreply, State}.~n"
    "terminate(_Reason, _State) -> timer:sleep(~p).~n".

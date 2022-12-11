-module(time_required_to_restart).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["generate-gen-server", NStr, MStr]) ->
    {VertexNum, EdgeNum} = parameters:get_vertex_num_and_edge_num(list_to_integer(NStr)),
    Vertices =
        lists:map(fun erlang:list_to_atom/1,
                  lists:map(fun erlang:integer_to_list/1, lists:seq(1, VertexNum))),
    Graph = adjacency_list:create_randomly(Vertices, EdgeNum),
    file:make_dir("src/"),
    file:make_dir("src/gen_servers/"),
    maps:foreach(fun(Name, Code) ->
                    file:write_file(
                        io_lib:format("src/gen_servers/~s.erl", [Name]), Code)
                 end,
                 maps:map(fun(From, Tos) ->
                             DelayTime = parameters:get_delay_time(list_to_integer(MStr)),
                             gen_server_generator:generate(From, Tos, DelayTime)
                          end,
                          Graph)),
    erlang:halt();
main(["measure", NStr, MStr]) ->
    % NOTE: When a gen_server is initialized, a message is sent to this name.
    register(measurer, self()),
    N = list_to_integer(NStr),
    {ok, Pid} = supervisor:start_link(bean, []),
    unlink(Pid),
    % NOTE: Omit supervision reports
    logger:add_handler_filter(default, ?MODULE, {fun(_, _) -> stop end, nostate}),
    GenServerNames = create_gen_server_names(N),
    {VertexNum, EdgeNum} = parameters:get_vertex_num_and_edge_num(N),
    DelayTime = parameters:get_delay_time(list_to_integer(MStr)),
    MaxRestartTime = 2 * DelayTime * VertexNum,
    MeasurementTime =
        average(lists:map(fun(_) -> measure_time_to_restart(GenServerNames, MaxRestartTime) end,
                          lists:seq(1, parameters:get_loop_num()))),
    io:format("VertexNum, EdgeNum, DelayTime: RestartTime (Average of ~p repetitions)~n",
              [parameters:get_loop_num()]),
    io:format("~p, ~p, ~p: ~p~n", [VertexNum, EdgeNum, DelayTime, MeasurementTime]),
    erlang:halt();
main(Args) ->
    io:format("Illegal options: ~p~n", [Args]),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================
create_gen_server_names(N) ->
    {VertexNum, _} = parameters:get_vertex_num_and_edge_num(N),
    lists:map(fun(X) -> list_to_atom(integer_to_list(X)) end, lists:seq(1, VertexNum)).

measure_time_to_restart(GenServerNames, MaxRestartTime) ->
    SelectedGenServerName =
        lists:nth(
            rand:uniform(length(GenServerNames)), GenServerNames),
    StopTime = calendar:local_time(),
    gen_server:stop(SelectedGenServerName),
    % NOTE: Wait for the reboot complete.
    flush_until_timeout(MaxRestartTime),
    StartTime = get_start_time(SelectedGenServerName),
    {_Days, Time} = calendar:time_difference(StopTime, StartTime),
    calendar:time_to_seconds(Time).

-spec get_start_time(sys:name()) -> calendar:datetime().
get_start_time(Name) ->
    {ok, Statistics} = sys:statistics(Name, get),
    {start_time, StartTime} = lists:keyfind(start_time, 1, Statistics),
    StartTime.

-spec average([number()]) -> non_neg_integer().
average(NumList) ->
    Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, NumList),
    Sum / length(NumList).

flush_until_timeout(Timeout) ->
    receive _ -> flush_until_timeout(Timeout) after Timeout -> ok end.

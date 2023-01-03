-module(time_required_to_restart).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["generate-gen-server" | StrParameters]) ->
    [VertexNum, EdgeNum, DelayTime] = lists:map(fun erlang:list_to_integer/1, StrParameters),
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
                 maps:map(fun(From, Tos) -> gen_server_generator:generate(From, Tos, DelayTime) end,
                          Graph)),
    erlang:halt();
main(["measure" | StrParameters]) ->
    [VertexNum, _EdgeNum, DelayTime] = lists:map(fun erlang:list_to_integer/1, StrParameters),
    % NOTE: When a gen_server is initialized, a message is sent to this name.
    register(measurer, self()),
    {ok, Pid} = supervisor:start_link(bean, []),
    unlink(Pid),
    % NOTE: Omit supervision reports
    logger:add_handler_filter(default, ?MODULE, {fun(_, _) -> stop end, nostate}),
    GenServerNames = create_gen_server_names(VertexNum),
    MaxRestartTime = DelayTime * VertexNum,
    AvgRestartTime =
        average(lists:map(fun(Name) -> measure_time_to_restart(Name, MaxRestartTime) end,
                          GenServerNames)),
    io:format("~p~n", [AvgRestartTime]),
    erlang:halt();
main(["calculate-cost"]) ->
    Modules = find_source_files("src/bean"),
    SupNames =
        lists:map(fun(M) ->
                     {ok, CModule} = dialyzer_utils:get_core_from_src(M),
                     {c_literal, _, ModuleName} = cerl:module_name(CModule),
                     ModuleName
                  end,
                  Modules),
    Sups =
        lists:map(fun(Name) ->
                     {ok, SupSpec} = apply(Name, init, [[]]),
                     {Name, SupSpec}
                  end,
                  SupNames),
    io:format("~p~n",
              [supervision_tree:calc_cost(
                   supervision_tree:from_supervisor_specs(Sups))]),
    erlang:halt();
main(Args) ->
    io:format("Illegal options: ~p~n", [Args]),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================
create_gen_server_names(VertexNum) ->
    lists:map(fun(X) -> list_to_atom(integer_to_list(X)) end, lists:seq(1, VertexNum)).

measure_time_to_restart(GenServerName, MaxRestartTime) ->
    StopTime = calendar:local_time(),
    gen_server:stop(GenServerName),
    % NOTE: Wait for the reboot complete.
    flush_until_timeout(MaxRestartTime),
    StartTime = get_start_time(GenServerName),
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

find_source_files(Dir) ->
    filelib:fold_files(Dir, ".*\.erl", true, fun(X, Acc) -> [X | Acc] end, []).

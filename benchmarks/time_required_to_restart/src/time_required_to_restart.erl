-module(time_required_to_restart).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["generate-gen-server", NStr]) ->
    list_to_integer(NStr),
    % TODO: 実際は gen_server を自動生成する
    erlang:halt();
main(["measure", NStr]) ->
    N = list_to_integer(NStr),
    {VertexNum, EdgeNum} = get_vertex_num_and_edge_num(N),
    {ok, Pid} = supervisor:start_link(bean, []),
    unlink(Pid),
    GenServerNames = create_gen_server_names(N),
    % MeasurementTime = measure_time_to_restart(GenServerNames),
    MeasurementTime =
        average(lists:map(fun(_) -> measure_time_to_restart(GenServerNames) end,
                          lists:seq(1, 5))),
    io:format("~p, ~p: ~p~n", [VertexNum, EdgeNum, MeasurementTime]),
    erlang:halt();
main(Args) ->
    % 1. gen_server の集合を作る
    % 2. 監視ツリーを作る
    % NOTE: ここまでの作業は同じ escript ないでできないから外でやる
    % 3. 監視ツリーを起動する
    % 4. 再起動時間を計測する
    %    a. ランダムに gen_server を選択する
    %    b. 選択した gen_server を停止する
    %    c. 1秒ほど待つ
    %    d. 選択した gen_server が再起動にかかった時間を調べる
    %    e. a -- d の手続きを50回ほど繰り返し，再起動にかかった時間の平均を取る
    % 5. 自動生成した gen_server と監視ツリーを削除する
    % GenServerNames = generate_gen_servers(),
    % RootSupervisorName = run_bean(),
    % {ok, Pid} = supervisor:start_link(RootSupervisorName, []),
    % {ok, Pid} = supervisor:start_link(bean, []),
    % unlink(Pid),
    % StopTime = calendar:local_time(),
    % gen_server:stop('3'),
    % timer:sleep(1000),
    % StartTime = get_start_time('3'),
    % Time = calendar:time_difference(StopTime, StartTime),
    % io:format("StopTime: ~p, StartTime: ~p~nTime: ~p~n", [StopTime, StartTime, Time]),
    io:format("Illegal options: ~p~n", [Args]),

    % finalize(),
    erlang:halt(1).

% init(Number) ->
%     generate_gen_servers(),
%     run_bean().

%%====================================================================
%% Internal functions
%%====================================================================
get_vertex_num_and_edge_num(1) -> {3, 2};
get_vertex_num_and_edge_num(2) -> {10, 5}.

create_gen_server_names(N) ->
    {VertexNum, _} = get_vertex_num_and_edge_num(N),
    lists:map(fun(X) -> list_to_atom(integer_to_list(X)) end, lists:seq(1, VertexNum)).

measure_time_to_restart(GenServerNames) ->
    SelectedGenServerName =
        lists:nth(
            rand:uniform(length(GenServerNames)), GenServerNames),
    StopTime = calendar:local_time(),
    gen_server:stop(SelectedGenServerName),
    timer:sleep(1000),
    StartTime = get_start_time(SelectedGenServerName),
    {_Days, Time} = calendar:time_difference(StopTime, StartTime),
    calendar:time_to_seconds(Time).

% generate_gen_servers() ->
%     % TODO: 実際は gen_server を自動生成する
%     ['1', '2', '3'].

% run_bean() ->
%     os:cmd("rebar3 bean"),
%     % NOTE: `bean` is the name of a root supervisor in a supervision tree generated by `bean`
%     bean.

% finalize() -> os:cmd("rm -rf src/bean").

-spec get_start_time(sys:name()) -> calendar:datetime().
get_start_time(Name) ->
    {ok, Statistics} = sys:statistics(Name, get),
    {start_time, StartTime} = lists:keyfind(start_time, 1, Statistics),
    StartTime.

-spec average([number()]) -> non_neg_integer().
average(NumList) ->
    Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, NumList),
    Sum / length(NumList).

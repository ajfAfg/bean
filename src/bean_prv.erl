-module(bean_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bean).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER},            % The 'user friendly' name of the task
                          {module, ?MODULE},            % The module implementation of the task
                          {bare,
                           true},                 % The task can be run by the user, always true
                          {deps, ?DEPS},                % The list of dependencies
                          {example, "rebar3 bean"}, % How to use the plugin
                          {opts,
                           bean_opts()},                   % list of options understood by the plugin
                          {short_desc, "A rebar plugin"},
                          {desc, "A rebar plugin"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    FunOpt =
        case proplists:get_value(algorithm, Args, polynomial_time_without_correctness) of
            polynomial_time_without_correctness ->
                {some,
                 fun all_local_minimum_vertex_splitters_solver:solve_in_polynomial_time_without_correctness/1};
            exp_time_with_correctness ->
                {some,
                 fun all_local_minimum_vertex_splitters_solver:solve_in_exp_time_with_correctness/1};
            _ -> none
        end,
    case FunOpt of
        {some, TakeAllLocalMinimumVertexSplitters} ->
            lists:foreach(fun(App) -> generate_supervisor(App, TakeAllLocalMinimumVertexSplitters)
                          end,
                          rebar_state:project_apps(State)),
            {ok, State};
        none -> {error, "Invalid algorithm"}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) -> io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
-spec generate_supervisor(rebar_app_info:t(),
                          all_local_minimum_vertex_splitters_solver:take_all_local_minimum_vertex_splitters()) ->
                             ok.
generate_supervisor(_App, TakeAllLocalMinimumVertexSplitters) ->
    Modules = find_source_files("src"),
    CModules =
        lists:map(fun(M) ->
                     {ok, CModule} = dialyzer_utils:get_core_from_src(M),
                     CModule
                  end,
                  Modules),
    case dependency_extractor:extract(CModules) of
        none -> ok;
        {some, DependencyGraph} ->
            SupSpecs =
                supervisor_specs_constructor:construct(
                    optimum_supervision_tree_solver:solve(DependencyGraph,
                                                          TakeAllLocalMinimumVertexSplitters)),
            Format =
                "-module('~s').~n"
                "-behavior(supervisor).~n"
                "-export([init/1]).~n"
                "~n"
                "init(_Args) ->"
                "SupFlags = ~p,"
                "ChildSpecs = ~p,"
                "{ok, {SupFlags, ChildSpecs}}.~n",
            SupStrsWithName =
                lists:map(fun(#{name := Name,
                                sup_flags := SupFlags,
                                child_specs := ChildSpecs}) ->
                             {Name, io_lib:format(Format, [Name, SupFlags, ChildSpecs])}
                          end,
                          SupSpecs),
            file:make_dir("src/"),
            file:make_dir("src/bean/"),
            lists:foreach(fun({Name, SupStr}) ->
                             file:write_file(
                                 io_lib:format("src/bean/~s.erl", [Name]), SupStr)
                          end,
                          SupStrsWithName)
    end.

find_source_files(Dir) ->
    filelib:fold_files(Dir, ".*\.erl", true, fun(X, Acc) -> [X | Acc] end, []).

bean_opts() ->
    [{algorithm,
      $a,
      "algorithm",
      atom,
      "Algorithm used to generate a supervision tree. "
      "Choice of `polynomial_time_without_correctness` and `exp_time_with_correctness`. "
      "[default: polynomial_time_without_correctness]"}].

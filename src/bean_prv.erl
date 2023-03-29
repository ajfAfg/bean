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
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc, "A rebar plugin"},
                          {desc, "A rebar plugin"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

% TODO: handle errors
% -spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    lists:foreach(fun generate_supervisor/1, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) -> io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
generate_supervisor(_App) ->
    Modules = find_source_files("src"),
    CModules =
        lists:map(fun(M) ->
                     {ok, CModule} = dialyzer_utils:get_core_from_src(M),
                     CModule
                  end,
                  Modules),
    SupSpecs =
        supervisor_specs_constructor:construct(
            optimum_supervision_tree_solver:solve(
                dependency_extractor:extract_dependencies(CModules))),
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
                  SupStrsWithName).

find_source_files(Dir) ->
    filelib:fold_files(Dir, ".*\.erl", true, fun(X, Acc) -> [X | Acc] end, []).

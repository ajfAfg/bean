-module(bean_prv).

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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Generate =
        fun(_App) ->
           Paths =
               rebar_dir:src_dirs(
                   dict:new(), ["src"]),
           Modules = find_source_files(Paths),
           CModules =
               lists:map(fun(M) ->
                            {ok, CModule} = dialyzer_utils:get_core_from_src(M),
                            CModule
                         end,
                         Modules),
           supervision_tree_generator:generate(CModules)
        end,

    lists:foreach(Generate, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) -> io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
find_source_files(Paths) -> find_source_files(Paths, []).

find_source_files([], Files) -> Files;
find_source_files([Path | Rest], Files) ->
    find_source_files(Rest,
                      [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.erl", Path)]
                      ++ Files).

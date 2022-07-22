-module(bean_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([{?MODULE, do, 1}]).
-ignore_xref([{?MODULE, format_error, 1}]).

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
    {Name, ChildSpecs} = supervision_tree_constructor:construct(CModules),
    Format =
        "-module(~s).~n"
        "-behavior(supervisor).~n"
        "-export([start_link/0]).~n"
        "-export([init/1]).~n"
        "~n"
        "start_link() -> supervisor:start_link(~s, []).~n"
        "init(_Args) ->"
        "SupFlags = #{},"
        "ChildSpecs = ~p,"
        "{ok, {SupFlags, ChildSpecs}}.~n",
    SupStr = io_lib:format(Format, [Name, Name, ChildSpecs]),
    file:write_file(
        io_lib:format("src/~s.erl", [Name]), SupStr).

find_source_files(Paths) -> find_source_files(Paths, []).

find_source_files([], Files) -> Files;
find_source_files([Path | Rest], Files) ->
    find_source_files(Rest,
                      [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.erl", Path)]
                      ++ Files).

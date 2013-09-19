%% @author Erick Gonzalez <erick@swellimagination.com>
%% @doc The ETBX library is a set of Erlang ([http://www.erlang.org]) functions to perform common tasks that are a recurrent theme during development of Erlang applications.

-module(etbx).
-vsn("1.0.0").
-export([get_env/1, get_env/2]).
-export([maybe_apply/3, maybe_apply/4]).
-export([set_loglevel/1]).
-export([start_app/1]).
-export([stop_app/1]).

%% @doc Retrieves an Application env setting
-spec get_env(atom()) -> any().
get_env(K)->
    get_env(K, undefined).

%% @doc Retrieves a Hypervisor Application env setting
-spec get_env(atom(), any()) -> any().
get_env(K, Default) ->
    case application:get_env(K) of
        {ok, V} -> V;
        undefined -> Default
    end.

%% @doc Call the specified function if it exists, otherwise just don't and
%% return undefined
-spec maybe_apply(module(), function(), list()) -> any().
maybe_apply(Mod, Fun, Args) ->
    maybe_apply(Mod, Fun, Args, undefined).

%% @doc Call the specified function if it exists, otherwise just don't and
%% return the given Return parameter instead
-spec maybe_apply(module(), function(), list(), any()) -> any().
maybe_apply(Mod, Fun, Args, Return) ->
    case catch apply(Mod, Fun, Args) of 
        {'EXIT', {undef, _}} -> Return;
        Unknown -> Unknown
    end.

%% @doc Set lager logging level at runtime. Specify one of debug, info,
%% notice, warning, error, critical, alert or emergency.
-spec set_loglevel(lager:log_level()) -> true.
set_loglevel(Level) ->
    lager:set_loglevel(lager_console_backend, Level).

%% @private
stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    lager:debug([{event, application}], "Stopping ~s~n", [App]),
    application:stop(App),
    stop_apps(Apps).

%% @private
start(App) ->
    application:start(App).

%% @private
start_dep(App, {error, {not_started, Dep}}, Apps) ->
    lager:debug([{event, application}], "Starting ~s~n", [Dep]),
    {ok, _, DApps} = start_dep(Dep, start(Dep), Apps),
    start_dep(App, start(App), DApps);
start_dep(App, {error, {already_started, App}}, Apps) ->
    {ok, App, Apps};
start_dep(App, ok, Apps) ->
    lager:debug([{event, applictation}], "~s start ok~n", [App]),
    {ok, App, [App | Apps]}.

%% @doc Starts all dependencies for a given application and then the
%% application itself. It returns an application start token that can
%% be used later to stop the application and all dependencies
-spec start_app(atom()) -> {ok, atom(), list() } | any().
start_app(App) ->
    start_dep(App, start(App), []).

%% @doc Stops an application and all its dependencies. The start token
%% that needs to be provided here is the one returned by start_app()
-spec stop_app({ok, atom(), list()}) -> ok | any().
stop_app(Token) ->
    stop_apps(Token).

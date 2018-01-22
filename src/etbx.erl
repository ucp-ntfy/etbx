%% @author Erick Gonzalez <erick@swellimagination.com>
%% @doc The ETBX library is a set of Erlang ([http://www.erlang.org]) functions to perform common tasks that are a recurrent theme during development of Erlang applications.

-module(etbx).
-vsn("1.0.0").
-export([contains/2]).
-export([get_env/1, get_env/2]).
-export([index_of/2]).
-export([is_nil/0, is_nil/1]).
-export([maybe_apply/3, maybe_apply/4]).
-export([update/3]).
-export([run/1, run/2]).
-export([set_loglevel/1]).
-export([start_app/1]).
-export([stop_app/1]).
-export([to_atom/1, to_atom/2]).
-export([to_binary/1]).
-export([to_list/1]).
-export([to_rec/2]).
-export([to_string/1]).

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
-spec(stop_app({ok, atom(), list()} | [string()]) -> ok | any()).
stop_app({ok, _, Apps}) ->
    stop_apps(Apps);
stop_app(Apps) ->
    stop_apps(Apps).

%% @doc a common sense test for what one would expect should be a
%% "nil" value. Seriously Ericsson.
-spec is_nil(any())  -> boolean().
is_nil(undefined) -> true;
is_nil([])        -> true;
is_nil({})        -> true;
is_nil(<<>>)      -> true;
is_nil(_)         -> false.
is_nil()          -> true.

-type proplist()::list(tuple()).

%% @private
index_of(_, [], _) -> undefined;
index_of(X, [H | T], I) ->
    if H =:= X -> I;
       true    -> index_of(X, T, I+1)
    end.

%% @doc returns the index for the first occurrence of an element in a list
%% or undefined if the element is not in the list
-spec index_of(any(), list()) -> number() | undefined.
index_of(X, L) ->
    index_of(X, L, 0).
%% @doc converts a property list into a record.

to_rec({R, [_ | N], Spec}, P) when is_atom(R) and is_list(Spec) ->
    list_to_tuple(
      [R | lists:foldl(
             fun ({K,V}, A) ->
                     case index_of(K, Spec) of
                         undefined ->
                             A;
                         I ->
                             {Head, Tail} = lists:split(I, A),
                             Rest = case Tail of
                                        [_ | M] -> M;
                                        []      -> []
                                    end,
                             Head ++ [V | Rest]
                     end
             end, N, P)]).

%% @doc Tests if X is present in the given list.
-spec contains(any(), list()) -> boolean().
contains(_, []) -> false;
contains(X, [{K,_} | T]) ->
    if X =:= K -> true;
       true    -> contains(X, T)
    end;
contains(X, [H | T]) ->
    if X =:= H -> true;
       true    -> contains(X, T)
    end.

%% @doc update property K with value V in proplist L
-spec update(any(), any(), proplist()) -> proplist().
update(K, V, []) ->
    [{K,V}];
update(K, V, [{_,_}|_] = L) ->
    [{K, V} | proplists:delete(K, L)].

%%%=========================================================================
%%% Type conversion
%%%=========================================================================

to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_tuple(X) ->
    IsSet = sets:is_set(X),
    if IsSet ->
        sets:to_list(X);
    true ->
        tuple_to_list(X)
    end;
to_list(X) when is_number(X) ->
    to_string(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.
to_string(X) when is_integer(X) ->
    integer_to_list(X);
to_string(X) when is_float(X) ->
    float_to_list(X);
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_list(X) ->
    X.
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_integer(X) ->
    <<X>>;
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, latin1);
to_binary(X) when is_binary(X) ->
    X.
to_atom(X) when is_list(X) ->
    list_to_existing_atom(X);
to_atom(X) when is_binary(X) ->
    binary_to_existing_atom(X, latin1);
to_atom(X) when is_number(X) ->
    to_atom(to_string(X));
to_atom(X) when is_atom(X) ->
    X.
to_atom(X, unsafe) when is_list(X) ->
    list_to_atom(X);
to_atom(X, unsafe) when is_binary(X) ->
    binary_to_atom(X, latin1);
to_atom(X, unsafe) when is_number(X) ->
    to_atom(to_string(X), unsafe);
to_atom(X, unsafe) when is_atom(X) ->
    X.

%%%=========================================================================
%%% Running Shell Commands
%%%=========================================================================

run(Cmd) ->
    run(Cmd, 5000).

run(Cmd, Timeout) ->
    Port = open_port({spawn, Cmd}, [exit_status]),
    run(Port, <<>>, Timeout).

run(Port, Data, Timeout) ->
    receive {Port, {data, NewData}}  ->
                NewBin = to_binary(NewData),
                run(Port, <<Data/binary,NewBin/binary>>, Timeout);
            {Port, {exit_status, 0}} -> {ok, Data};
            {Port, {exit_status, S}} -> throw({error, S})
    after Timeout
              -> throw(timeout)
    end.



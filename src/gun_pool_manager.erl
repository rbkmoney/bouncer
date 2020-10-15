-module(gun_pool_manager).

%% API functions

-export([start_link/0]).
-export([acquire_connection/3]).

%% Gen Server callbacks
-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(SERVER, ?MODULE).

%% Internal types

-type conn_host() :: gun_pool:conn_host().
-type conn_port() :: gun_pool:conn_port().
-type conn_opts() :: gun_pool_connection:client_opts().
-type connection() :: gun_pool_connection:connection().

-type state() :: #{
    pools := pool_map()
}.

-type pool_key() :: {conn_host(), conn_port()}.
-type pool_t() :: [connection()].
-type pool_map() :: #{
    pool_key() => pool_t()
}.

%%

-define(DEFAULT_POOL_SIZE, 5). % @TODO

%%
%% API functions
%%

-spec start_link() ->
    genlib_gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec acquire_connection(conn_host(), conn_port(), timeout()) ->
    {ok, connection()} | {error, _Reason} | no_return().
acquire_connection(Host, Port, Timeout) ->
    gen_server:call(?SERVER, {acquire_connection, Host, Port}, Timeout).


%%
%% Gen Server callbacks
%%

-spec init([]) ->
    {ok, state()}.
init([]) ->
    {ok, new_state()}.

-spec handle_call(any(), _From, state()) ->
    {reply, {ok, connection()} | {error, _Reason}, state()}.
handle_call({acquire_connection, Host, Port}, _From, St0) ->
    case do_acquire_connection(Host, Port, St0) of
        {ok, Connection, St1} ->
            {reply, {ok, Connection}, St1};
        {error, _Reason} = Error ->
            {reply, Error, St0}
    end;
handle_call(_Call, _From, _St) ->
    erlang:error(unexpected_call).

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_Cast, St) ->
    {noreply, St}.

-spec handle_info(any(), state()) ->
    {noreply, state()}.
handle_info({'DOWN', _Mref, process, ConnectionPid, _Reason}, St0) ->
    St1 = unregister_connection(ConnectionPid, St0),
    {noreply, St1}.

%%
%% Internal functions
%%

-spec new_state() ->
    state().
new_state() ->
    #{
        pools => #{}
    }.

-spec pool_key(conn_host(), conn_port()) ->
    pool_key().
pool_key(Host, Port) ->
    {Host, Port}.

-spec get_pool(pool_key(), state()) ->
    pool_t() | undefined.
get_pool(PoolKey, #{pools := Pools}) ->
    maps:get(PoolKey, Pools, undefined).

-spec set_pool(pool_key(), pool_t(), state()) ->
    state().
set_pool(PoolKey, Pool, #{pools := Pools0} = State) ->
    Pools1 = Pools0#{PoolKey => Pool},
    State#{pools => Pools1}.

%%

-spec do_acquire_connection(conn_host(), conn_port(), state()) ->
    {ok, connection(), state()} | {error, _Reason}.
do_acquire_connection(Host, Port, St0) ->
    PoolKey = pool_key(Host, Port),
    case try_get_connection(PoolKey, St0) of
        {ok, Connection} ->
            % @TODO we can also implement different (configurable) connection retrieval algorithms
            % (f.e. get connection with least amount of requests in its "queue")
            St1 = rotate_pool(PoolKey, St0),
            {ok, Connection, St1};
        {error, no_pool} ->
            case create_pool(Host, Port, PoolKey, St0) of
                {ok, St1} ->
                    do_acquire_connection(Host, Port, St1); % @TODODO
                {error, _} = Error ->
                    Error
            end
    end.

-spec try_get_connection(pool_key(), state()) ->
    {ok, connection()} | {error, no_pool}.
try_get_connection(PoolKey, St0) ->
    case get_pool(PoolKey, St0) of
        Pool when is_list(Pool) ->
            {ok, get_next_connection(Pool)}; % @TODO there can be no connections left
        undefined ->
            {error, no_pool}
    end.

-spec get_next_connection(pool_t()) ->
    connection().
get_next_connection([Connection | _]) ->
    Connection.

-spec rotate_pool(pool_key(), state()) ->
    state().
rotate_pool(PoolKey, St0) ->
    Pool0 = get_pool(PoolKey, St0),
    Pool1 = rotate_pool_connections(Pool0),
    % My C-senses are tingling, maybe instead of physically shuffling the list just store a rotating index?
    set_pool(PoolKey, Pool1, St0).

-spec rotate_pool_connections(pool_t()) ->
    pool_t().
rotate_pool_connections([H | T]) ->
    T ++ [H].

-spec create_pool(conn_host(), conn_port(), pool_key(), state()) ->
    {ok, state()} | {error, _Reason}.
create_pool(Host, Port, PoolKey, St0) ->
    % @TODO configurable pool creation
    try
        Opts = get_connection_opts(St0),
        Pool = create_connections(Host, Port, Opts, ?DEFAULT_POOL_SIZE),
        {ok, set_pool(PoolKey, Pool, St0)}
    catch
        throw:Reason:_St ->
            {error, Reason}
    end.

-spec create_connections(conn_host(), conn_port(), conn_opts(), non_neg_integer()) ->
    [connection()] | no_return().
create_connections(Host, Port, Opts, Count) ->
    create_connections(Host, Port, Opts, Count, []).

-spec create_connections(conn_host(), conn_port(), conn_opts(), non_neg_integer(), [connection()]) ->
    [connection()] | no_return().
create_connections(_, _, _, 0, PoolAcc) ->
    PoolAcc;
create_connections(Host, Port, Opts, Count, PoolAcc) ->
    Connection = create_connection(Host, Port, Opts),
    create_connections(Host, Port, Opts, Count - 1, [Connection | PoolAcc]).

-spec create_connection(conn_host(), conn_port(), conn_opts()) ->
    connection() | no_return().
create_connection(Host, Port, Opts) ->
    case gun_pool_connection_sup:start_connection(Host, Port, Opts) of
        {ok, Connection} ->
            _ = erlang:monitor(process, Connection),
            Connection;
        {error, Error} ->
            erlang:throw(Error)
    end.

-spec get_connection_opts(state()) ->
    conn_opts().
get_connection_opts(_St) ->
    % @TODO actually pass params from app config
    #{
        connect_timeout => 5000
    }.

-spec unregister_connection(connection(), state()) ->
    state().
unregister_connection(ConnectionPid, #{pools := Pools0} = St0) ->
    Pools1 = maps:map(
        fun(_PoolKey, Connections) ->
            lists:delete(ConnectionPid, Connections)
        end,
        Pools0
    ),
    St0#{pools := Pools1}.

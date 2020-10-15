-module(gun_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API functions
%%
-spec start_link() ->
    genlib_gen:start_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% Supervisor callbacks
%%
-spec init(Args :: term()) ->
    genlib_gen:supervisor_ret().
init([]) ->
    SupFlags = #{
        strategy => one_for_all
    },
    Children = [
        #{
            id => gun_pool_connection_sup,
            start => {gun_pool_connection_sup, start_link, []},
            type => supervisor,
            restart => permanent,
            shutdown => brutal_kill %% @TODO some grace
        },
        #{
            id => gun_pool_manager,
            start => {gun_pool_manager, start_link, []},
            type => worker,
            restart => permanent,
            shutdown => brutal_kill %% @TODO some grace
        }
    ],
    {ok, {SupFlags, Children}}.

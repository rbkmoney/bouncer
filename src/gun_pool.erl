-module(gun_pool).

%% API functions

-export([request/8]).

%% API types

-type conn_host() :: inet:hostname() | inet:ip_address().
-type conn_port() :: inet:port_number().
-type req_path() :: iodata().
-type req_method() :: binary().
-type req_headers() :: gun:req_headers().
-type req_opts() :: gun:req_opts().
-type body() :: binary().

-type response() :: gun_pool_connection:response_data().

-export_type([
    conn_host/0,
    conn_port/0,
    req_path/0,
    req_method/0,
    req_headers/0,
    req_opts/0,
    body/0,
    response/0
]).


%%
%% API functions
%%

-spec request(conn_host(), conn_port(), req_path(), req_method(), req_headers(), body(), req_opts(), timeout()) ->
    {ok, response()} | {error, _Reason}.
request(Host, Port, Path, Method, ReqHeaders, Body, ReqOpts, Timeout) ->
    case gun_pool_manager:acquire_connection(Host, Port, Timeout) of
        {ok, Connection} ->
            gun_pool_connection:request(Connection, Path, Method, ReqHeaders, Body, ReqOpts, Timeout);
        {error, _} = Error ->
            Error
    end.

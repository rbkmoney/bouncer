-module(bouncer_opa_client).

%% API Functions

-export([init/0]).
-export([request_document/3]).

%% API Types

-type opts() :: #{
    pool_opts := gunner:pool_opts(),
    endpoint := gunner:endpoint()
}.

-type client() :: #{
    endpoint := gunner:endpoint(),
    connection_pool := gunner:pool()
}.

-type ruleset_id() :: iodata().

-type document() ::
    null
    | binary()
    | number()
    | boolean()
    | #{atom() | binary() => document()}
    | [document()].

-export_type([client/0]).
-export_type([ruleset_id/0]).
-export_type([document/0]).

%%

-define(DEFAULT_REQUEST_TIMEOUT, 1000).

%%
%% API Functions
%%

-spec init(opts()) -> client().
init(OpaClientOpts) ->
    {ok, PoolPid} = gunner:start_pool(maps:get(pool_opts, OpaClientOpts)),
    #{
        endpoint => maps:get(endpoint, OpaClientOpts),
        connection_pool => PoolPid,
        request_timeout => get_request_timeout(PoolOpts)
    }.

-spec request_document(_ID :: iodata(), _Input :: document(), client()) ->
    {ok, document()}
    | {error,
        notfound
        | {unavailable, _Reason}
        | {unknown, _Reason}}.
request_document(RulesetID, Input, Client) ->
    #{endpoint := Endpoint, connection_pool := PoolPid, request_timeout := Timeout} = Client,
    Path = join_path(<<"/v1/data">>, join_path(RulesetID, <<"/judgement">>)),
    % TODO
    % A bit hacky, ordsets are allowed in context and supposed to be opaque, at least by design.
    % We probably need something like `bouncer_context:to_json/1`.
    Body = jsx:encode(#{input => Input}),
    CType = <<"application/json; charset=utf-8">>,
    Headers = #{
        <<"content-type">> => CType,
        <<"accept">> => CType
    },
    %% TODO this is ugly
    try
        Deadline = erlang:monotonic_time(millisecond) + Timeout,
        StreamRef = do_request(PoolPid, Endpoint, Path, Headers, Body),
        TimeoutLeft0 = Deadline - erlang:monotonic_time(millisecond),
        Result =
            case gunner:await(StreamRef, TimeoutLeft0) of
                {response, nofin, 200, _Headers} ->
                    TimeoutLeft1 = Deadline - erlang:monotonic_time(millisecond),
                    case gunner:await_body(StreamRef, TimeoutLeft1) of
                        {ok, Response, _Trailers} ->
                            decode_document(Response);
                        {ok, Response} ->
                            decode_document(Response);
                        {error, Reason} ->
                            {error, {unknown, Reason}}
                    end;
                {response, fin, 404, _Headers} ->
                    {error, notfound};
                {error, Reason} ->
                    {error, {unknown, Reason}}
            end,
        _ = gunner:free(PoolPid, StreamRef),
        Result
    catch
        throw:({unavailable, _} = Error) ->
            {error, Error}
    end.

-spec decode_document(binary()) -> {ok, document()} | {error, notfound}.
decode_document(Response) ->
    case jsx:decode(Response) of
        #{<<"result">> := Result} ->
            {ok, Result};
        #{} ->
            {error, notfound}
    end.

%%

do_request(PoolPid, Endpoint, Path, Headers, Body) ->
    case gunner:post(PoolPid, Endpoint, Path, Headers, Body) of
        {ok, StreamRef} ->
            StreamRef;
        {error, Reason} ->
            throw({unavailable, Reason})
    end.

-spec get_request_timeout(gunner:pool_opts()) -> timeout().
get_request_timeout(PoolOpts) ->
    ClientOpts = maps:get(connection_opts, PoolOpts, #{}),
    maps:get(request_timeout, ClientOpts, ?DEFAULT_REQUEST_TIMEOUT).

%%

join_path(F1, F2) when is_binary(F1), is_binary(F2) ->
    normalize_path(genlib_string:cat(normalize_path(F1), normalize_path(F2))).

normalize_path(P = <<$/, P1/binary>>) ->
    S1 = byte_size(P1),
    case S1 > 0 andalso binary:last(P1) of
        $/ -> binary:part(P, 0, S1);
        _ -> P
    end;
normalize_path(P) when is_binary(P) ->
    normalize_path(<<$/, P/binary>>);
normalize_path(P) ->
    normalize_path(iolist_to_binary(P)).

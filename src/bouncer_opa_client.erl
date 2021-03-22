-module(bouncer_opa_client).

%% API Functions

-export([init/1]).
-export([request_document/3]).

%% API Types

-type opts() :: #{
    pool_opts := gunner:pool_opts(),
    endpoint := gunner:endpoint(),
    dns_resolver_ip_picker => gunner_resolver:ip_picker()
}.

-opaque client() :: #{
    endpoint := gunner:endpoint(),
    connection_pool := gunner:pool(),
    request_timeout := timeout(),
    dns_resolver_ip_picker => gunner_resolver:ip_picker()
}.

-type ruleset_id() :: iodata().

-type document() ::
    null
    | binary()
    | number()
    | boolean()
    | #{atom() | binary() => document()}
    | [document()].

-export_type([opts/0]).
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
    PoolOpts = maps:get(pool_opts, OpaClientOpts),
    {ok, PoolPid} = gunner:start_pool(PoolOpts),
    genlib_map:compact(#{
        endpoint => maps:get(endpoint, OpaClientOpts),
        connection_pool => PoolPid,
        request_timeout => get_request_timeout(PoolOpts),
        dns_resolver_ip_picker => maps:get(dns_resolver_ip_picker, OpaClientOpts, undefined)
    }).

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
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    case gunner_resolver:resolve_endpoint(Endpoint, make_resolver_opts(Client)) of
        {ok, ResolvedEndpoint} ->
            %% Trying the synchronous API first
            TimeoutLeft = Deadline - erlang:monotonic_time(millisecond),
            %%@TODO but what about acquire_timeout?
            GunnerOpts = #{request_timeout => TimeoutLeft},
            case gunner:post(PoolPid, ResolvedEndpoint, Path, Body, Headers, GunnerOpts) of
                {ok, 200, _, Response} when is_binary(Response) ->
                    decode_document(Response);
                {ok, 404, _, _} ->
                    {error, notfound};
                {ok, Code, _, Response} ->
                    {error, {unknown, {Code, Response}}};
                {error, {unknown, Reason}} ->
                    {error, {unknown, Reason}};
                {error, Reason} ->
                    {error, {unavailable, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
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

make_resolver_opts(#{request_timeout := Timeout, dns_resolver_ip_picker := IpPicker}) ->
    #{timeout => Timeout, ip_picker => IpPicker};
make_resolver_opts(#{request_timeout := Timeout}) ->
    #{timeout => Timeout}.

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

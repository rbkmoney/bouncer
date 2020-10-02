-module(bouncer_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([missing_ruleset_notfound/1]).
-export([incorrect_ruleset_invalid/1]).
-export([missing_content_invalid_context/1]).
-export([junk_content_invalid_context/1]).
-export([conflicting_context_invalid/1]).
-export([distinct_sets_context_valid/1]).

-export([allowed_create_invoice_shop_manager/1]).
-export([forbidden_w_empty_context/1]).
-export([forbidden_expired/1]).

-export([connect_failed_means_unavailable/1]).
-export([connect_timeout_means_unavailable/1]).
-export([request_timeout_means_unknown/1]).

-behaviour(bouncer_arbiter_pulse).
-export([handle_beat/3]).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").

-type config() :: [{atom(), term()}].
-type group_name() :: atom().
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

%%

-define(OPA_HOST, "opa").
-define(OPA_ENDPOINT, {?OPA_HOST, 8181}).
-define(API_RULESET_ID, "authz/api").

-spec all() ->
    [atom()].

all() ->
    [
        {group, general},
        {group, network_error_mapping}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        % {general, [parallel], [
        {general, [], [
            missing_ruleset_notfound,
            incorrect_ruleset_invalid,
            missing_content_invalid_context,
            junk_content_invalid_context,
            conflicting_context_invalid,
            distinct_sets_context_valid
        ]},
        {rules_authz_api, [], [
            allowed_create_invoice_shop_manager,
            forbidden_expired,
            forbidden_w_empty_context
        ]},
        {network_error_mapping, [], [
            connect_failed_means_unavailable,
            connect_timeout_means_unavailable,
            request_timeout_means_unknown
        ]}
    ].

-spec init_per_suite(config()) ->
    config().

init_per_suite(C) ->
    Apps =
        genlib_app:start_application(woody) ++
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_logger}
        ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) ->
    ok.
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(Name, C) when
    Name == general;
    Name == rules_authz_api
->
    start_bouncer([], [{groupname, Name} | C]);
init_per_group(Name, C) ->
    [{groupname, Name} | C].

start_bouncer(Env, C) ->
    IP = "127.0.0.1",
    Port = 8022,
    ArbiterPath = <<"/v1/arbiter">>,
    Apps = genlib_app:start_application_with(bouncer, [
        {ip, IP},
        {port, Port},
        {services, #{
            arbiter => #{
                path  => ArbiterPath,
                pulse => {?MODULE, []}
            }
        }},
        {transport_opts, #{
            max_connections => 1000,
            num_acceptors   => 4
        }},
        {opa, #{
            endpoint  => ?OPA_ENDPOINT,
            transport => tcp
            % event_handler => {ct_gun_event_h, []}
        }}
    ] ++ Env),
    Services = #{
        arbiter => mk_url(IP, Port, ArbiterPath)
    },
    [{group_apps, Apps}, {service_urls, Services} | C].

mk_url(IP, Port, Path) ->
    iolist_to_binary(["http://", IP, ":", genlib:to_binary(Port), Path]).

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Name, C) ->
    stop_bouncer(C).

stop_bouncer(C) ->
    with_config(group_apps, C, fun (Apps) -> genlib_app:stop_unload_applications(Apps) end).

-spec init_per_testcase(atom(), config()) ->
    config().

init_per_testcase(Name, C) ->
    C1 = [{testcase, Name} | C],
    [{client, mk_client(C1)} | C1].

-spec end_per_testcase(atom(), config()) ->
    config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-define(bdcs_context(Fragments), #bdcs_Context{fragments = Fragments}).
-define(bdcs_assertion(Code), #bdcs_Assertion{code = Code}).
-define(bdcs_judgement(Resolution), #bdcs_Judgement{resolution = Resolution}).
-define(bdcs_judgement(Resolution, Assertions),
    #bdcs_Judgement{resolution = Resolution, assertions = Assertions}
).

-spec missing_ruleset_notfound(config()) -> ok.
-spec incorrect_ruleset_invalid(config()) -> ok.
-spec missing_content_invalid_context(config()) -> ok.
-spec junk_content_invalid_context(config()) -> ok.
-spec conflicting_context_invalid(config()) -> ok.
-spec distinct_sets_context_valid(config()) -> ok.

missing_ruleset_notfound(C) ->
    MissingRulesetID = "missing_ruleset",
    ?assertThrow(
        #bdcs_RulesetNotFound{},
        call_judge(MissingRulesetID, ?bdcs_context(#{}), ?config(client, C))
    ).

incorrect_ruleset_invalid(C) ->
    ?assertThrow(
        #bdcs_InvalidRuleset{},
        call_judge("trivial/incorrect1", ?bdcs_context(#{}), ?config(client, C))
    ),
    ?assertThrow(
        #bdcs_InvalidRuleset{},
        call_judge("trivial/incorrect2", ?bdcs_context(#{}), ?config(client, C))
    ).

missing_content_invalid_context(C) ->
    NoContentFragment = #bctx_ContextFragment{type = v1_thrift_binary},
    ?assertThrow(
        #bdcs_InvalidContext{},
        call_judge(?API_RULESET_ID, ?bdcs_context(#{<<"missing">> => NoContentFragment}), ?config(client, C))
    ).

junk_content_invalid_context(C) ->
    Junk = <<"STOP RIGHT THERE YOU CRIMINAL SCUM!">>,
    JunkFragment = #bctx_ContextFragment{type = v1_thrift_binary, content = Junk},
    ?assertThrow(
        #bdcs_InvalidContext{},
        call_judge(?API_RULESET_ID, ?bdcs_context(#{<<"missing">> => JunkFragment}), ?config(client, C))
    ).

conflicting_context_invalid(C) ->
    Fragment1 = #{
        user => #{
            id    => <<"joeblow">>,
            email => <<"deadinside69@example.org">>
        },
        requester => #{
            ip => <<"1.2.3.4">>
        }
    },
    Fragment2 = #{
        user => #{
            id    => <<"joeblow">>,
            email => <<"deadinside420@example.org">>
        },
        requester => #{
            ip => <<"1.2.3.4">>
        }
    },
    Context = ?bdcs_context(#{
        <<"frag1">> => mk_ctx_v1_fragment(Fragment1),
        <<"frag2">> => mk_ctx_v1_fragment(Fragment2)
    }),
    ?assertThrow(
        #bdcs_InvalidContext{},
        call_judge(?API_RULESET_ID, Context, ?config(client, C))
    ).

distinct_sets_context_valid(C) ->
    Fragment1 = #{
        user => #{
            id   => <<"joeblow">>,
            orgs => mk_ordset([
                #{
                    id => <<"hoolie">>,
                    roles => mk_ordset([#{id => <<"Administrator">>}])
                },
                #{
                    id => <<"weewrok">>,
                    roles => mk_ordset([#{id => <<"Administrator">>}])
                }
            ])
        }
    },
    Fragment2 = #{
        user => #{
            id   => <<"joeblow">>,
            orgs => mk_ordset([
                #{
                    id => <<"hoolie">>,
                    roles => mk_ordset([#{id => <<"Nobody">>}])
                },
                #{
                    id => <<"blooply">>,
                    roles => mk_ordset([#{id => <<"Nobody">>}])
                }
            ])
        }
    },
    Context = ?bdcs_context(#{
        <<"frag1">> => mk_ctx_v1_fragment(Fragment1),
        <<"frag2">> => mk_ctx_v1_fragment(Fragment2)
    }),
    ?assertMatch(
        #bdcs_Judgement{},
        call_judge(?API_RULESET_ID, Context, ?config(client, C))
    ).

%%

-spec allowed_create_invoice_shop_manager(config()) -> ok.
-spec forbidden_expired(config()) -> ok.
-spec forbidden_w_empty_context(config()) -> ok.

allowed_create_invoice_shop_manager(C) ->
    Fragment = lists:foldl(fun maps:merge/2, #{}, [
        mk_auth_session_token(),
        mk_env(),
        mk_op_create_invoice(<<"BLARG">>, <<"SHOP">>, <<"PARTY">>),
        mk_user(<<"USER">>, mk_ordset([
            mk_user_org(<<"PARTY">>, <<"OWNER">>, mk_ordset([
                mk_role(<<"Manager">>, <<"SHOP">>)
            ]))
        ]))
    ]),
    Context = ?bdcs_context(#{<<"root">> => mk_ctx_v1_fragment(Fragment)}),
    ?assertMatch(
        ?bdcs_judgement(allowed, [?bdcs_assertion(<<"user_has_role">>)]),
        call_judge(?API_RULESET_ID, Context, ?config(client, C))
    ).

forbidden_expired(C) ->
    % Would be funny if this fails on some system too deep in the past.
    Fragment = maps:merge(mk_env(), #{
        auth => #{
            method     => <<"AccessToken">>,
            expiration => <<"1991-12-26T17:00:00Z">> % ☭😢
        }
    }),
    Context = ?bdcs_context(#{<<"root">> => mk_ctx_v1_fragment(Fragment)}),
    ?assertMatch(
        ?bdcs_judgement(forbidden, [?bdcs_assertion(<<"auth_expired">>)]),
        call_judge(?API_RULESET_ID, Context, ?config(client, C))
    ).

forbidden_w_empty_context(C) ->
    EmptyFragment = mk_ctx_v1_fragment(#{}),
    ?assertMatch(
        ?bdcs_judgement(forbidden, [?bdcs_assertion(<<"auth_required">>)]),
        call_judge(?API_RULESET_ID, ?bdcs_context(#{}), ?config(client, C))
    ),
    ?assertMatch(
        ?bdcs_judgement(forbidden, [?bdcs_assertion(<<"auth_required">>)]),
        call_judge(?API_RULESET_ID, ?bdcs_context(#{<<"empty">> => EmptyFragment}), ?config(client, C))
    ).

mk_user(UserID, UserOrgs) ->
    #{user => #{
        id   => UserID,
        orgs => UserOrgs
    }}.

mk_user_org(OrgID, OwnerID, Roles) ->
    #{
        id => OrgID,
        owner => #{id => OwnerID},
        roles => Roles
    }.

mk_role(RoleID) ->
    #{id => RoleID}.

mk_role(RoleID, ShopID) ->
    #{id => RoleID, scope => #{shop => #{id => ShopID}}}.

mk_auth_session_token() ->
    mk_auth_session_token(erlang:system_time(second) + 3600).

mk_auth_session_token(ExpiresAt) ->
    #{auth => #{
        method     => <<"SessionToken">>,
        expiration => format_ts(ExpiresAt, second)
    }}.

mk_op_create_invoice(InvoiceID, ShopID, PartyID) ->
    #{capi => #{
        op => #{
            id      => <<"CreateInvoice">>,
            invoice => #{id => InvoiceID},
            shop    => #{id => ShopID},
            party   => #{id => PartyID}
        }
    }}.

mk_env() ->
    #{env => #{
        now => format_now()
    }}.

format_now() ->
    USec = erlang:system_time(second),
    format_ts(USec, second).

format_ts(Ts, Unit) ->
    Str = calendar:system_time_to_rfc3339(Ts, [{unit, Unit}, {offset, "Z"}]),
    erlang:list_to_binary(Str).

%%

-spec connect_failed_means_unavailable(config()) -> ok.
-spec connect_timeout_means_unavailable(config()) -> ok.
-spec request_timeout_means_unknown(config()) -> ok.

connect_failed_means_unavailable(C) ->
    C1 = start_bouncer([{opa, #{
        endpoint => {?OPA_HOST, 65535},
        transport => tcp,
        event_handler => {ct_gun_event_h, []}
    }}], C),
    Client = mk_client(C1),
    try
        ?assertError(
            {woody_error, {external, resource_unavailable, _}},
            call_judge(?API_RULESET_ID, ?bdcs_context(#{}), Client)
        )
    after
        stop_bouncer(C1)
    end.

connect_timeout_means_unavailable(C) ->
    {ok, Proxy} = ct_proxy:start_link(?OPA_ENDPOINT, #{listen => ignore}),
    C1 = start_bouncer([{opa, #{
        endpoint => ct_proxy:endpoint(Proxy),
        transport => tcp,
        event_handler => {ct_gun_event_h, []}
    }}], C),
    Client = mk_client(C1),
    try
        ?assertError(
            % NOTE
            % Turns out it's somewhat hard to simulate connection timeout when connecting to the
            % localhost. This is why we expect `result_unknown` here instead of
            % `resource_unavailable`.
            {woody_error, {external, result_unknown, _}},
            call_judge(?API_RULESET_ID, ?bdcs_context(#{}), Client)
        )
    after
        stop_bouncer(C1)
    end.

request_timeout_means_unknown(C) ->
    {ok, Proxy} = ct_proxy:start_link(?OPA_ENDPOINT),
    C1 = start_bouncer([{opa, #{
        endpoint => ct_proxy:endpoint(Proxy),
        transport => tcp,
        event_handler => {ct_gun_event_h, []}
    }}], C),
    Client = mk_client(C1),
    ok = change_proxy_mode(Proxy, connection, ignore, C),
    try
        ?assertError(
            {woody_error, {external, result_unknown, _}},
            call_judge(?API_RULESET_ID, ?bdcs_context(#{}), Client)
        )
    after
        stop_bouncer(C1)
    end.

change_proxy_mode(Proxy, Scope, Mode, C) ->
    ModeWas = ct_proxy:mode(Proxy, Scope, Mode),
    _ = ct:pal(debug, "[~p] set proxy ~p from '~p' to '~p'", [?config(testcase, C), Scope, ModeWas, Mode]),
    ok.

%%

mk_ordset(L) ->
    ordsets:from_list(L).

mk_ctx_v1_fragment(Context) ->
    {ok, Content} = bouncer_context_v1:encode(thrift, Context),
    #bctx_ContextFragment{type = v1_thrift_binary, content = Content}.

%%

mk_client(C) ->
    WoodyCtx = woody_context:new(genlib:to_binary(?config(testcase, C))),
    ServiceURLs = ?config(service_urls, C),
    {WoodyCtx, ServiceURLs}.

call_judge(RulesetID, Context, Client) ->
    call(arbiter, 'Judge', {genlib:to_binary(RulesetID), Context}, Client).

call(ServiceName, Fn, Args, {WoodyCtx, ServiceURLs}) ->
    Service = get_service_spec(ServiceName),
    Opts = #{
        url => maps:get(ServiceName, ServiceURLs),
        event_handler => scoper_woody_event_handler
    },
    case woody_client:call({Service, Fn, Args}, Opts, WoodyCtx) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.

get_service_spec(arbiter) ->
    {bouncer_decisions_thrift, 'Arbiter'}.

%%

-spec handle_beat(bouncer_arbiter_pulse:beat(), bouncer_arbiter_pulse:metadata(), []) ->
    ok.
handle_beat(Beat, Metadata, []) ->
    ct:pal("~p [arbiter] ~0p:~nmetadata=~p", [self(), Beat, Metadata]).

%%

-spec with_config(atom(), config(), fun ((_) -> R)) ->
    R | undefined.
with_config(Name, C, Fun) ->
    case lists:keyfind(Name, 1, C) of
        {_, V} -> Fun(V);
        false  -> undefined
    end.

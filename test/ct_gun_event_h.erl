-module(ct_gun_event_h).
-behavior(gun_event).

-export([init/2]).
-export([domain_lookup_start/2]).
-export([domain_lookup_end/2]).
-export([connect_start/2]).
-export([connect_end/2]).
-export([tls_handshake_start/2]).
-export([tls_handshake_end/2]).
-export([request_start/2]).
-export([request_headers/2]).
-export([request_end/2]).
-export([push_promise_start/2]).
-export([push_promise_end/2]).
-export([response_start/2]).
-export([response_inform/2]).
-export([response_headers/2]).
-export([response_trailers/2]).
-export([response_end/2]).
-export([ws_upgrade/2]).
-export([ws_recv_frame_start/2]).
-export([ws_recv_frame_header/2]).
-export([ws_recv_frame_end/2]).
-export([ws_send_frame_start/2]).
-export([ws_send_frame_end/2]).
-export([protocol_changed/2]).
-export([transport_changed/2]).
-export([origin_changed/2]).
-export([cancel/2]).
-export([disconnect/2]).
-export([terminate/2]).

-type st() :: _.

%% NOTE
%% See https://github.com/ninenines/gun/pull/249.

-spec init(_Event :: map(), st()) ->
    st().
init(Event, State) ->
    _ = ct:pal("~p [gun] init: ~p", [self(), Event]),
    State.

-spec domain_lookup_start(_Event :: map(), st()) ->
    st().
domain_lookup_start(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup start: ~p", [self(), Event]),
    State.

-spec domain_lookup_end(_Event :: map(), st()) ->
    st().
domain_lookup_end(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup end: ~p", [self(), Event]),
    State.

-spec connect_start(_Event :: map(), st()) ->
    st().
connect_start(Event, State) ->
    _ = ct:pal("~p [gun] connect start: ~p", [self(), Event]),
    State.

-spec connect_end(_Event :: map(), st()) ->
    st().
connect_end(Event, State) ->
    _ = ct:pal("~p [gun] connect end: ~p", [self(), Event]),
    State.

-spec tls_handshake_start(_Event :: map(), st()) ->
    st().
tls_handshake_start(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake start: ~p", [self(), Event]),
    State.

-spec tls_handshake_end(_Event :: map(), st()) ->
    st().
tls_handshake_end(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake end: ~p", [self(), Event]),
    State.

-spec request_start(_Event :: map(), st()) ->
    st().
request_start(Event, State) ->
    _ = ct:pal("~p [gun] request start: ~p", [self(), Event]),
    State.

-spec request_headers(_Event :: map(), st()) ->
    st().
request_headers(Event, State) ->
    _ = ct:pal("~p [gun] request headers: ~p", [self(), Event]),
    State.

-spec request_end(_Event :: map(), st()) ->
    st().
request_end(Event, State) ->
    _ = ct:pal("~p [gun] request end: ~p", [self(), Event]),
    State.

-spec push_promise_start(_Event :: map(), st()) ->
    st().
push_promise_start(Event, State) ->
    _ = ct:pal("~p [gun] push promise start: ~p", [self(), Event]),
    State.

-spec push_promise_end(_Event :: map(), st()) ->
    st().
push_promise_end(Event, State) ->
    _ = ct:pal("~p [gun] push promise end: ~p", [self(), Event]),
    State.

-spec response_start(_Event :: map(), st()) ->
    st().
response_start(Event, State) ->
    _ = ct:pal("~p [gun] response start: ~p", [self(), Event]),
    State.

-spec response_inform(_Event :: map(), st()) ->
    st().
response_inform(Event, State) ->
    _ = ct:pal("~p [gun] response inform: ~p", [self(), Event]),
    State.

-spec response_headers(_Event :: map(), st()) ->
    st().
response_headers(Event, State) ->
    _ = ct:pal("~p [gun] response headers: ~p", [self(), Event]),
    State.

-spec response_trailers(_Event :: map(), st()) ->
    st().
response_trailers(Event, State) ->
    _ = ct:pal("~p [gun] response trailers: ~p", [self(), Event]),
    State.

-spec response_end(_Event :: map(), st()) ->
    st().
response_end(Event, State) ->
    _ = ct:pal("~p [gun] response end: ~p", [self(), Event]),
    State.

-spec ws_upgrade(_Event :: map(), st()) ->
    st().
ws_upgrade(Event, State) ->
    _ = ct:pal("~p [gun] ws upgrade: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_start(_Event :: map(), st()) ->
    st().
ws_recv_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame start: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_header(_Event :: map(), st()) ->
    st().
ws_recv_frame_header(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame header: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_end(_Event :: map(), st()) ->
    st().
ws_recv_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame end: ~p", [self(), Event]),
    State.

-spec ws_send_frame_start(_Event :: map(), st()) ->
    st().
ws_send_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame start: ~p", [self(), Event]),
    State.

-spec ws_send_frame_end(_Event :: map(), st()) ->
    st().
ws_send_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame end: ~p", [self(), Event]),
    State.

-spec protocol_changed(_Event :: map(), st()) ->
    st().
protocol_changed(Event, State) ->
    _ = ct:pal("~p [gun] protocol changed: ~p", [self(), Event]),
    State.

-spec transport_changed(_Event :: map(), st()) ->
    st().
transport_changed(Event, State) ->
    _ = ct:pal("~p [gun] transport changed: ~p", [self(), Event]),
    State.

-spec origin_changed(_Event :: map(), st()) ->
    st().
origin_changed(Event, State) ->
    _ = ct:pal("~p [gun] origin changed: ~p", [self(), Event]),
    State.

-spec cancel(_Event :: map(), st()) ->
    st().
cancel(Event, State) ->
    _ = ct:pal("~p [gun] cancel: ~p", [self(), Event]),
    State.

-spec disconnect(_Event :: map(), st()) ->
    st().
disconnect(Event, State) ->
    _ = ct:pal("~p [gun] disconnect: ~p", [self(), Event]),
    State.

-spec terminate(_Event :: map(), st()) ->
    st().
terminate(Event, State) ->
    _ = ct:pal("~p [gun] terminate: ~p", [self(), Event]),
    State.

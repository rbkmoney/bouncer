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

init(Event, State) ->
    _ = ct:pal("~p [gun] init: ~p", [self(), Event]),
    State.

domain_lookup_start(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup start: ~p", [self(), Event]),
    State.

domain_lookup_end(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup end: ~p", [self(), Event]),
    State.

connect_start(Event, State) ->
    _ = ct:pal("~p [gun] connect start: ~p", [self(), Event]),
    State.

connect_end(Event, State) ->
    _ = ct:pal("~p [gun] connect end: ~p", [self(), Event]),
    State.

tls_handshake_start(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake start: ~p", [self(), Event]),
    State.

tls_handshake_end(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake end: ~p", [self(), Event]),
    State.

request_start(Event, State) ->
    _ = ct:pal("~p [gun] request start: ~p", [self(), Event]),
    State.

request_headers(Event, State) ->
    _ = ct:pal("~p [gun] request headers: ~p", [self(), Event]),
    State.

request_end(Event, State) ->
    _ = ct:pal("~p [gun] request end: ~p", [self(), Event]),
    State.

push_promise_start(Event, State) ->
    _ = ct:pal("~p [gun] push promise start: ~p", [self(), Event]),
    State.

push_promise_end(Event, State) ->
    _ = ct:pal("~p [gun] push promise end: ~p", [self(), Event]),
    State.

response_start(Event, State) ->
    _ = ct:pal("~p [gun] response start: ~p", [self(), Event]),
    State.

response_inform(Event, State) ->
    _ = ct:pal("~p [gun] response inform: ~p", [self(), Event]),
    State.

response_headers(Event, State) ->
    _ = ct:pal("~p [gun] response headers: ~p", [self(), Event]),
    State.

response_trailers(Event, State) ->
    _ = ct:pal("~p [gun] response trailers: ~p", [self(), Event]),
    State.

response_end(Event, State) ->
    _ = ct:pal("~p [gun] response end: ~p", [self(), Event]),
    State.

ws_upgrade(Event, State) ->
    _ = ct:pal("~p [gun] ws upgrade: ~p", [self(), Event]),
    State.

ws_recv_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame start: ~p", [self(), Event]),
    State.

ws_recv_frame_header(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame header: ~p", [self(), Event]),
    State.

ws_recv_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame end: ~p", [self(), Event]),
    State.

ws_send_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame start: ~p", [self(), Event]),
    State.

ws_send_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame end: ~p", [self(), Event]),
    State.

protocol_changed(Event, State) ->
    _ = ct:pal("~p [gun] protocol changed: ~p", [self(), Event]),
    State.

transport_changed(Event, State) ->
    _ = ct:pal("~p [gun] transport changed: ~p", [self(), Event]),
    State.

origin_changed(Event, State) ->
    _ = ct:pal("~p [gun] origin changed: ~p", [self(), Event]),
    State.

cancel(Event, State) ->
    _ = ct:pal("~p [gun] cancel: ~p", [self(), Event]),
    State.

disconnect(Event, State) ->
    _ = ct:pal("~p [gun] disconnect: ~p", [self(), Event]),
    State.

terminate(Event, State) ->
    _ = ct:pal("~p [gun] terminate: ~p", [self(), Event]),
    State.

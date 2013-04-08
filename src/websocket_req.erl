%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
%% Accessor module for the #websocket_req{} record.
%%
%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
-module(websocket_req).

-include("websocket_client.hrl").

-export([protocol/1,
         host/1,
         port/1,
         path/1,
         keepalive/1,
         socket/1,
         transport/1,
         handler/1,
         key/1,
         remaining/1,
         opcode/1]).

-spec protocol(websocket_req()) -> protocol().
protocol(#websocket_req{protocol = P}) -> P.


-spec host(websocket_req()) -> string().
host(#websocket_req{host = H}) -> H.


-spec port(websocket_req()) -> inet:port_number().
port(#websocket_req{port = P}) -> P.


-spec path(websocket_req()) ->  string().
path(#websocket_req{path = P}) -> P.


-spec keepalive(websocket_req()) -> integer().
keepalive(#websocket_req{keepalive = K}) -> K.


-spec socket(websocket_req()) -> inet:socket() | ssl:sslsocket().
socket(#websocket_req{socket = S}) -> S.


-spec transport(websocket_req()) -> module().
transport(#websocket_req{transport = T}) -> T.


-spec handler(websocket_req()) -> module().
handler(#websocket_req{handler = H}) -> H.


-spec key(websocket_req()) -> binary().
key(#websocket_req{key = K}) -> K.


-spec remaining(websocket_req()) -> undefined | integer().
remaining(#websocket_req{remaining = R}) -> R.

-spec opcode(websocket_req()) -> opcode().
opcode(#websocket_req{opcode = O}) -> O.

%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
%% Accessor module for the #websocket_req{} record.
%%
%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
-module(websocket_req).

-include("websocket_client.hrl").

-record(websocket_req, {
          protocol  :: protocol(),
          host      :: string(),
          port      :: inet:port_number(),
          path      :: string(),
          keepalive :: integer(),
          socket    :: inet:socket() | ssl:sslsocket(),
          transport :: module(),
          handler   :: module(),
          key       :: binary(),
          remaining :: undefined | integer(),
          opcode    :: opcode()
         }).

-opaque req() :: #websocket_req{}.
-export_type([req/0]).


-export([new/1,
         protocol/2,   protocol/1,
         host/2,       host/1,
         port/2,       port/1,
         path/2,       path/1,
         keepalive/2,  keepalive/1,
         socket/2,     socket/1,
         transport/2,  transport/1,
         handler/2,    handler/1,
         key/2,        key/1,
         remaining/2,  remaining/1,
         opcode/2,     opcode/1
        ]).

-spec new([tuple()]) -> req().
new(Args) ->
    new(Args, #websocket_req{}).

new([], Req) -> Req;
new([{protocol , Val} | Props], Req) -> new(Props, Req#websocket_req{protocol   = Val}); 
new([{host     , Val} | Props], Req) -> new(Props, Req#websocket_req{host       = Val}); 
new([{port     , Val} | Props], Req) -> new(Props, Req#websocket_req{port       = Val}); 
new([{path     , Val} | Props], Req) -> new(Props, Req#websocket_req{path       = Val}); 
new([{keepalive, Val} | Props], Req) -> new(Props, Req#websocket_req{keepalive  = Val}); 
new([{socket   , Val} | Props], Req) -> new(Props, Req#websocket_req{socket     = Val}); 
new([{transport, Val} | Props], Req) -> new(Props, Req#websocket_req{transport  = Val}); 
new([{handler  , Val} | Props], Req) -> new(Props, Req#websocket_req{handler    = Val}); 
new([{key      , Val} | Props], Req) -> new(Props, Req#websocket_req{key        = Val}); 
new([{remaining, Val} | Props], Req) -> new(Props, Req#websocket_req{remaining  = Val}); 
new([{opcode   , Val} | Props], Req) -> new(Props, Req#websocket_req{opcode     = Val}).


-spec protocol(req()) -> protocol().
protocol(#websocket_req{protocol = P}) -> P.

-spec protocol(protocol(), req()) -> req().
protocol(P, Req) ->
	Req#websocket_req{protocol = P}.


-spec host(req()) -> string().
host(#websocket_req{host = H}) -> H.

-spec host(string(), req()) -> req().
host(H, Req) ->
	Req#websocket_req{host = H}.


-spec port(req()) -> inet:port_number().
port(#websocket_req{port = P}) -> P.

-spec port(inet:port_number(), req()) -> req().
port(P, Req) ->
	Req#websocket_req{port = P}.


-spec path(req()) -> string().
path(#websocket_req{path = P}) -> P.

-spec path(string(), req()) -> req().
path(P, Req) ->
	Req#websocket_req{path = P}.


-spec keepalive(req()) -> integer().
keepalive(#websocket_req{keepalive = K}) -> K.

-spec keepalive(integer(), req()) -> req().
keepalive(K, Req) ->
	Req#websocket_req{keepalive = K}.


-spec socket(req()) -> inet:socket() | ssl:sslsocket().
socket(#websocket_req{socket = S}) -> S.

-spec socket(inet:socket() | ssl:sslsocket(), req()) -> req().
socket(S, Req) ->
	Req#websocket_req{socket = S}.


-spec transport(req()) -> module().
transport(#websocket_req{transport = T}) -> T.

-spec transport(module(), req()) -> req().
transport(T, Req) ->
	Req#websocket_req{transport = T}.


-spec handler(req()) -> module().
handler(#websocket_req{handler = H}) -> H.

-spec handler(module(), req()) -> req().
handler(H, Req) ->
	Req#websocket_req{handler = H}.


-spec key(req()) -> binary().
key(#websocket_req{key = K}) -> K.

-spec key(binary(), req()) -> req().
key(K, Req) ->
	Req#websocket_req{key = K}.


-spec remaining(req()) -> undefined | integer().
remaining(#websocket_req{remaining = R}) -> R.

-spec remaining(undefined | integer(), req()) -> req().
remaining(R, Req) ->
	Req#websocket_req{remaining = R}.


-spec opcode(req()) -> opcode().
opcode(#websocket_req{opcode = O}) -> O.

-spec opcode(opcode(), req()) -> req().
opcode(O, Req) ->
	Req#websocket_req{opcode = O}.


%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
%% Accessor module for the #websocket_req{} record.
%%
%% -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
-module(websocket_req).

-record(websocket_req, {
          protocol :: protocol(),
          host :: string(),
          port :: inet:port_number(),
          path :: string(),
          keepalive = infinity :: infinity | integer(),
          socket :: inet:socket() | ssl:sslsocket(),
          transport :: module(),
          handler :: module(),
          key :: binary(),
          remaining = undefined :: undefined | integer(),
          fin = undefined :: undefined | fin(),
          opcode = undefined :: undefined | opcode(),
          continuation = undefined :: undefined | binary(),
          continuation_opcode = undefined :: undefined | opcode()
         }).

-opaque req() :: #websocket_req{}.
-export_type([req/0]).

-type protocol() :: ws | wss.

-type frame() :: close | ping | pong
               | {text | binary | close | ping | pong, binary()}
               | {close, 1000..4999, binary()}.

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.
-export_type([protocol/0, opcode/0, frame/0]).

-type fin() :: 0 | 1.
-export_type([fin/0]).

-export([new/8,
         protocol/2, protocol/1,
         host/2, host/1,
         port/2, port/1,
         path/2, path/1,
         keepalive/2, keepalive/1,
         socket/2, socket/1,
         transport/2, transport/1,
         handler/2, handler/1,
         key/2, key/1,
         remaining/2, remaining/1,
         fin/2, fin/1,
         opcode/2, opcode/1,
         continuation/2, continuation/1,
         continuation_opcode/2, continuation_opcode/1,
         get/2, set/2
        ]).

-export([
         opcode_to_name/1,
         name_to_opcode/1
        ]).

-spec new(protocol(), string(), inet:port_number(),
          string(), inet:socket() | ssl:sslsocket(),
          module(), module(), binary()) -> req().
new(Protocol, Host, Port, Path, Socket, Transport, Handler, Key) ->
    #websocket_req{
     protocol = Protocol,
     host = Host,
     port = Port,
     path = Path,
     socket = Socket,
     transport = Transport,
     handler = Handler,
     key = Key
    }.


%% @doc Mapping from opcode to opcode name
-spec opcode_to_name(opcode()) ->
    atom().
opcode_to_name(0) -> continuation;
opcode_to_name(1) -> text;
opcode_to_name(2) -> binary;
opcode_to_name(8) -> close;
opcode_to_name(9) -> ping;
opcode_to_name(10) -> pong.

%% @doc Mapping from opcode to opcode name
-spec name_to_opcode(atom()) ->
    opcode().
name_to_opcode(continuation) -> 0;
name_to_opcode(text) -> 1;
name_to_opcode(binary) -> 2;
name_to_opcode(close) -> 8;
name_to_opcode(ping) -> 9;
name_to_opcode(pong) -> 10.


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

-spec fin(req()) -> fin().
fin(#websocket_req{fin = F}) -> F.

-spec fin(fin(), req()) -> req().
fin(F, Req) ->
    Req#websocket_req{fin = F}.

-spec opcode(req()) -> opcode().
opcode(#websocket_req{opcode = O}) -> O.

-spec opcode(opcode(), req()) -> req().
opcode(O, Req) ->
    Req#websocket_req{opcode = O}.

-spec continuation(req()) -> undefined | binary().
continuation(#websocket_req{continuation = C}) -> C.

-spec continuation(undefined | binary(), req()) -> req().
continuation(C, Req) ->
    Req#websocket_req{continuation = C}.

-spec continuation_opcode(req()) -> opcode().
continuation_opcode(#websocket_req{continuation_opcode = C}) -> C.

-spec continuation_opcode(opcode(), req()) -> req().
continuation_opcode(C, Req) ->
    Req#websocket_req{continuation_opcode = C}.


-spec get(atom(), req()) -> any(); ([atom()], req()) -> [any()].
get(List, Req) when is_list(List) ->
    [g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
    g(Atom, Req).

g(protocol, #websocket_req{protocol = Ret}) -> Ret;
g(host, #websocket_req{host = Ret}) -> Ret;
g(port, #websocket_req{port = Ret}) -> Ret;
g(path, #websocket_req{path = Ret}) -> Ret;
g(keepalive, #websocket_req{keepalive = Ret}) -> Ret;
g(socket, #websocket_req{socket = Ret}) -> Ret;
g(transport, #websocket_req{transport = Ret}) -> Ret;
g(handler, #websocket_req{handler = Ret}) -> Ret;
g(key, #websocket_req{key = Ret}) -> Ret;
g(remaining, #websocket_req{remaining = Ret}) -> Ret;
g(fin, #websocket_req{fin = Ret}) -> Ret;
g(opcode, #websocket_req{opcode = Ret}) -> Ret;
g(continuation, #websocket_req{continuation = Ret}) -> Ret;
g(continuation_opcode, #websocket_req{continuation_opcode = Ret}) -> Ret.


-spec set([{atom(), any()}], Req) -> Req when Req::req().
set([{protocol, Val} | Tail], Req) -> set(Tail, Req#websocket_req{protocol = Val});
set([{host, Val} | Tail], Req) -> set(Tail, Req#websocket_req{host = Val});
set([{port, Val} | Tail], Req) -> set(Tail, Req#websocket_req{port = Val});
set([{path, Val} | Tail], Req) -> set(Tail, Req#websocket_req{path = Val});
set([{keepalive, Val} | Tail], Req) -> set(Tail, Req#websocket_req{keepalive = Val});
set([{socket, Val} | Tail], Req) -> set(Tail, Req#websocket_req{socket = Val});
set([{transport, Val} | Tail], Req) -> set(Tail, Req#websocket_req{transport = Val});
set([{handler, Val} | Tail], Req) -> set(Tail, Req#websocket_req{handler = Val});
set([{key, Val} | Tail], Req) -> set(Tail, Req#websocket_req{key = Val});
set([{remaining, Val} | Tail], Req) -> set(Tail, Req#websocket_req{remaining = Val});
set([{fin, Val} | Tail], Req) -> set(Tail, Req#websocket_req{fin = Val});
set([{opcode, Val} | Tail], Req) -> set(Tail, Req#websocket_req{opcode = Val});
set([{continuation, Val} | Tail], Req) -> set(Tail, Req#websocket_req{continuation = Val});
set([{continuation_opcode, Val} | Tail], Req) -> set(Tail, Req#websocket_req{continuation_opcode = Val});
set([], Req) -> Req.

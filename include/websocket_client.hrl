-type protocol() :: ws | wss.

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.

-type frame() :: close | ping | pong |
  {text | binary | close | ping | pong, binary()}
  | {close, 1000..4999, binary()}.

-record(websocket_req, {
          protocol :: protocol(),
          host :: string(),
          port :: inet:port_number(),
          path ::  string(),
          keepalive :: integer(),
          socket :: inet:socket() | ssl:sslsocket(),
          transport :: module(),
          handler :: module(),
          key :: binary(),
          remaining :: undefined | integer(),
          opcode :: opcode()
         }).

-opaque websocket_req() :: #websocket_req{}.

-export_type([websocket_req/0, protocol/0, opcode/0, frame/0]).

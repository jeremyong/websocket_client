-module(websocket_client_handler).

-type state() :: any().
-type keepalive() :: integer().
-type close_type() :: normal | error | remote.

-callback init(list(), websocket_req:req()) ->
    {ok, state()}
        | {ok, state(), keepalive()}.

-callback websocket_handle({text | binary | ping | pong, binary()}, websocket_req:req(), state()) ->
    {ok, state()}
        | {reply, websocket_req:frame(), state()}
        | {close, binary(), state()}.

-callback websocket_info(any(), websocket_req:req(), state()) ->
    {ok, state()}
        | {reply, websocket_req:frame(), state()}
        | {close, binary(),  state()}.

-callback websocket_terminate({close_type(), binary()} | {close_type(), integer(), binary()},
                              websocket_req:req(), state()) ->
    ok.

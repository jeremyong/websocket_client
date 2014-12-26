-module(websocket_client_handler).

-type state() :: any().
-type keepalive() :: integer().
-type close_type() :: normal | error | remote.
-type delay() :: integer() | infinity.

-callback init(list(), state()) ->
    {ok, list(), state()}
        | {ok, list(), state(), delay()}.

-callback terminate(any(), state()) ->
    ok.

-callback handle_info(any(), state()) ->
    {noreply, state()}
        | {connect, state()}
        | {connect, list(), state()}
        | {stop, any(), state()}.

-callback websocket_init(list(), websocket_req:req()) ->
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

-callback websocket_terminate({close_type(), term()} | {close_type(), integer(), binary()},
                              websocket_req:req(), state()) ->
    ok.

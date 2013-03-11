-module(websocket_client_handler).

-type state() :: any().
-type keepalive() :: integer().

%% @doc spe
-callback init(list()) ->
    {ok, state()}
        | {ok, state(), keepalive()}.

-callback websocket_handle({text | binary | ping | pong, binary()}, State :: state()) ->
    {ok, State :: state()}
        | {reply, websocket_client:frame(), State :: state()}
        | {close, binary(), State :: state()}.

-callback websocket_info(any(), State :: state()) ->
    {ok, State :: state()}
        | {reply, websocket_client:frame(), State :: state()}
        | {close, binary(), State :: state()}.

-callback websocket_terminate({close, integer(), binary()}, _State :: state()) ->
    ok.

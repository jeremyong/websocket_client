-module(websocket_client_handler).

-callback init(list()) ->
    {ok, any()}.

-callback websocket_handle({text | binary | ping | pong, binary()}, State) ->
    {ok, State}
        | {reply, websocket_client:frame(), State}
        | {close, binary(), State}.

-callback websocket_info(any(), State) ->
    {ok, State}
        | {reply, websocket_client:frame(), State}
        | {close, binary(), State}.

-callback websocket_terminate({close, integer(), binary()}, _State) ->
    ok.

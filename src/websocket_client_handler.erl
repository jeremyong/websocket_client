-module(websocket_client_handler).

-callback init(list()) ->
    {ok, any()}.

-callback websocket_handle({text | binary, binary()}, State) ->
    {ok, State} | {reply, websocket_client:frame(), State}.

-callback websocket_info(any(), State) ->
    {ok, State} | {reply, websocket_client:frame(), State}.

-module(websocket_client_handler).

-type state() :: any().        %% Implementation state - controlled by implementation.
-type keepalive() :: integer().

%% @doc spe
-callback init(list(), websocket_req:req()) ->
    {ok, state()}
        | {ok, state(), keepalive()}.

-callback websocket_handle({text | binary | ping | pong, binary()}, websocket_req:req(), state()) ->
    {ok, state()}
        | {reply, websocket_client:frame(), state()}
        | {close, binary(), state()}.

-callback websocket_info(any(), websocket_req:req(), state()) ->
    {ok, state()}
        | {reply, websocket_client:frame(), state()}
        | {close, binary(),  state()}.

-callback websocket_terminate({close, integer(), binary()},  websocket_req:req(), state()) ->
    ok.

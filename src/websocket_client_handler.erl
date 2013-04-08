-module(websocket_client_handler).

-type state() :: any().        %% Implementation state - controlled by implementation.
-type conn_state() :: tuple(). %% Behaviour state  - controlled by websocket_client.erl
-type keepalive() :: integer().

%% @doc spe
-callback init(list(), conn_state()) ->
    {ok, conn_state(), state()}
        | {ok, conn_state(), state(), keepalive()}.

-callback websocket_handle({text | binary | ping | pong, binary()}, conn_state(), state()) ->
    {ok, conn_state(), state()}
        | {reply, websocket_client:frame(), conn_state(), state()}
        | {close, binary(), conn_state(), state()}.

-callback websocket_info(any(), conn_state(), state()) ->
    {ok, conn_state(), state()}
        | {reply, websocket_client:frame(), conn_state(), state()}
        | {close, binary(),  conn_state(), state()}.

-callback websocket_terminate({close, integer(), binary()},  conn_state(), state()) ->
    ok.

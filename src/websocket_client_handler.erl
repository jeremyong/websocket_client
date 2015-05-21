-module(websocket_client_handler).

-type state() :: any().
-type keepalive() :: integer().

-type close_reason() ::
    % Either the local end was closed via a `{closed, Reason, State}` tuple
    % returned from websocket_handle/3 or websocket_info/3, or the remote end
    % was closed during graceful teardown of the connection via a close frame
    % (see https://tools.ietf.org/html/rfc6455#section-5.5.1).
    {normal, binary()} |
    % The transport failed to send (see http://erlang.org/doc/man/gen_tcp.html#send-2
    % or http://erlang.org/doc/man/ssl.html#send-2)
    {error, term()} |
    % The remote end was closed due to a protocol error
    % (see https://tools.ietf.org/html/rfc6455#section-7.4.1, status code 1002).
    {error, badframe, binary()} |
    % The remote end was closed due to an encoding error
    % (see https://tools.ietf.org/html/rfc6455#section-7.4.1, status code 1007).
    {error, badencoding, binary()} |
    % The remote end was closed unexpectedly
    % (see https://tools.ietf.org/html/rfc6455#section-7.4.1, status code 1011).
    {error, handler, binary()} |
    % The remote host closed the socket.
    {remote, closed} |
    % The remote end was closed for some other reason
    % (see https://tools.ietf.org/html/rfc6455#section-7.4.1).
    {remote, integer(), binary()} |
    % The remote host closed the socket.
    % TODO: How is this different than `{remote, closed}`?
    {remote, binary()} |
    % The handler terminated unexpectedly in websocket_handle/3 or websocket_info/3.
    term().

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

-callback websocket_terminate(close_reason(), websocket_req:req(), state()) -> ok.

-module(websocket_client_handler).

-type state() :: any().
-type keepalive() :: integer().

-type close_reason() ::
    % Either:
    % - The websocket was closed by a handler via a `{closed, Reason, State}` tuple
    % returned from websocket_handle/3 or websocket_info/3.
    % - A 'close' frame was received with code 1000 or 1001.
    normal |
    % The local end failed to send (see http://erlang.org/doc/man/gen_tcp.html#send-2
    % or http://erlang.org/doc/man/ssl.html#send-2). The second element in the
    % tuple is the same term that was wrapped in an `{error, Reason}` tuple by
    % `send/2`, i.e. `{error, closed}` will become `{local, closed}`, and not
    % `{local, {error, closed}}`.
    {local, term()} |
    % The remote end either closed abruptly, or closed after sending a 'close' frame
    % without a status code.
    remote |
    % The remote end closed with a status code (see https://tools.ietf.org/html/rfc6455#section-7.4.1).
    {remote, integer()} |
    % An asynchronous exception was raised during message handling, either in
    % websocket_handle/3 or websocket_info/3. The term raised is passed as the
    % second element in this tuple.
    {handler, term()}.

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
        | {close, binary(), state()}.

-callback websocket_terminate(close_reason(), websocket_req:req(), state()) -> ok.

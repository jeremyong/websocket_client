# Erlang Websocket Client

## Existing features

1. Client to Server Masking
2. gen_server like callback behaviour
3. Handshake validation
4. tcp and ssl support
5. Handling of text, binary, ping, pong, and close frames

## Usage

For basic usage, please see the source files in the `examples/`
directory. Writing a handler is easy:

```erlang
-module(sample_ws_handler).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

start_link() ->
    websocket_client:start_link(?MODULE, ws, "echo.websocket.org", 80, "/", []).

init([]) ->
    self() ! start,
    {ok, 1}.

websocket_handle({text, Msg}, State) ->
    io:format("Received msg ~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, State + 1}.

websocket_info(start, State) ->
    {reply, {text, <<"this is message 1">>}, State + 1}.
```

The above code will send messages to the echo server that count up
from 1. It will also print all replies from the server:

```
Received msg <<"this is message 1">>
Received msg <<"hello, this is message #3">>
Received msg <<"hello, this is message #4">>
Received msg <<"hello, this is message #5">>
Received msg <<"hello, this is message #6">>
...
```

Erlang is typically used to write server applications. Now that
applications like `cowboy` supporting websocket applications are more
commonplace, it is important to have a compliant websocket client for
benchmarking and debugging purposes.

This client implements a cowboy like `websocket_client_handler` to
interact with a websocket server. Currently, it can connect via tcp or
ssl via the `ws` and `wss` protocols. It can also send and receive
contiguous text or binary websocket frames.

## TODO

The client as is is still missing a lot of functionality. We still
need to:

1. Handle continuation frames and fragmented messages
2. Close the connection in a number of error cases (malformed headers,
etc).
3. Add tests!! (hint hint)
4. Stop using `verify_none` by default
5. Provide additional arities of the `start_link` function to supply
a default port, path, protocol, or all the above

This is being released without the above functionality in case it is
useful as is for benchmarking or debugging as mentioned above (this is
the case for me). The hope is that the community (and me, as time
permits) will contribute key pieces to the codebase so that the major
pieces are taken care of.

# Erlang Websocket Client

## Existing features

1. Client to Server Masking
2. gen_server like callback behaviour
3. Handshake validation
4. tcp and ssl support
5. Handling of text, binary, ping, pong, and close frames
6. Handling of continuation frames

## Usage

For basic usage, please see the source files in the `examples/`
directory. Writing a handler is easy:

```erlang
-module(sample_ws_handler).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("wss://echo.websocket.org", ?MODULE, []).

init([], _ConnState) ->
    websocket_client:cast(self(), {text, <<"message 1">>}),
    {ok, 2}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle({text, Msg}, _ConnState, 5) ->
    io:format("Received msg ~p~n", [Msg]),
    {close, <<>>, 10};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg ~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, State + 1}.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
              [State, Code, Payload]),
    ok.
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

1. Close the connection in a number of error cases (malformed headers,
etc).
2. Add tests!! (hint hint)
3. Stop using `verify_none` by default

This is being released without the above functionality in case it is
useful as is for benchmarking or debugging as mentioned above (this is
the case for me). The hope is that the community (and me, as time
permits) will contribute key pieces to the codebase so that the major
pieces are taken care of.

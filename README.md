# Erlang Websocket Client

[![Build Status](https://travis-ci.org/jeremyong/websocket_client.svg?branch=master)](https://travis-ci.org/jeremyong/websocket_client)

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
         terminate/2,
         handle_info/2,
         websocket_init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("wss://echo.websocket.org", ?MODULE, []).


init([], WsUri) ->
    {ok, 2, WsUri}.

terminate(_Reason, _State) ->
    ok.

handle_info(_Msg, State) ->
    {noreply, State}.


websocket_init(StartCount, _ConnState) ->
    websocket_client:cast(self(), {text, <<"init message">>}),
    {ok, StartCount}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle({text, Msg}, _ConnState, 5 = State) ->
    io:format("Received msg: ~p; state: ~p~n", [Msg, State]),
    {close, <<>>, "done"};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg: ~p; state: ~p~n", [Msg, State]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, State + 1}.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    ok.
```

The above code will send messages to the echo server.
It will also print all replies from the server:

```
Received msg: <<"init message">>; state: 2
Received msg: <<"hello, this is message #2">>; state: 3
Received msg: <<"hello, this is message #3">>; state: 4
Received msg: <<"hello, this is message #4">>; state: 5
Websocket closed in state "done" wih reason {normal,<<>>}
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

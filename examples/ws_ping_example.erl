-module(ws_ping_example).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/1,
         websocket_handle/2,
         websocket_info/2,
         websocket_terminate/2
        ]).

start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("wss://echo.websocket.org", ?MODULE, []).

init([]) ->
    websocket_client:cast(self(), {text, <<"message 1">>}),
    %% Execute a ping every 1000 milliseconds
    {ok, 2, 1000}.

websocket_handle({pong, _Msg}, State) ->
    io:format("Received pong ~n"),
    {ok, State};
websocket_handle({text, Msg}, 5) ->
    io:format("Received msg ~p~n", [Msg]),
    {close, <<>>, 10};
websocket_handle({text, Msg}, State) ->
    io:format("Received msg ~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, State + 1}.

websocket_info(start, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate({close, Code, Payload}, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
              [State, Code, Payload]),
    ok.

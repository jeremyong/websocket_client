-module(sample_ws_handler).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

start_link() ->
    ssl:start(),
    crypto:start(),
    websocket_client:start_link(?MODULE, wss, "echo.websocket.org", 443, "/", []).

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

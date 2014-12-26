-module(async_start_example).

-behaviour(websocket_client_handler).

-export([
         no_delay/0,
         delay_5000/0,
         delay_infinity/0,
         delay_infinity_stop/0
        ]).
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

%% examples

no_delay() ->
    start_link().

delay_5000() ->
    start_link({delay, 5000}).

delay_infinity() ->
    {ok, Pid} = start_link({delay, infinity}),
    timer:sleep(5000),
    connect(Pid),
    {ok, Pid}.

delay_infinity_stop() ->
    {ok, Pid} = start_link({delay, infinity}),
    timer:sleep(5000),
    Pid ! stop,
    {ok, Pid}.



start_link() ->
    start_link([]).

start_link(Args) ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("wss://echo.websocket.org", ?MODULE, Args).

connect(Pid) ->
    Pid ! connect.


init({delay, Delay}, WsUri) ->
    io:format("Init with delay: ~p~n", [Delay]),
    {ok, 2, WsUri, Delay};
init(_, WsUri) ->
    io:format("Init without delay.~n"),
    {ok, 2, WsUri}.

terminate(Reason, State) ->
    io:format("Terminate in state ~p wih reason ~p~n", [State, Reason]),
    ok.

handle_info(connect, State) ->
    {connect, State};
handle_info(stop, State) ->
    {stop, stopped, State};
handle_info(_Msg, State) ->
    {noreply, State}.


websocket_init(StartCount, _ConnState) ->
    io:format("Websocket init with args: ~p~n", [StartCount]),
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

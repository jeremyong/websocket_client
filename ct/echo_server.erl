-module(echo_server).

-behaviour(cowboy_websocket_handler).

-export([
         start/0
        ]).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {}).

start() ->
    io:format("Starting echo server.~n"),
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, []}]}]),
    {ok, _} = cowboy:start_http(echo_listener, 2, [
                                                    {port, 8080},
                                                    {max_connections, 100}
                                                   ],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _Opts) ->
    {ok, Req, #state{}}.

websocket_handle(Frame, Req, State) ->
    io:format("Received frame~n"),
    {reply, Frame, Req, State}.

websocket_info(_Msg, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    io:format("Server terminating with reason ~p~n", [Reason]),
    ok.

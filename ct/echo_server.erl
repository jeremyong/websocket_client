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
    Dispatch = cowboy_router:compile([{'_', [
                                             {"/hello", ?MODULE, []},
                                             {'_', ?MODULE, []}
                                            ]}]),
    {ok, _} = cowboy:start_http(echo_listener, 2, [
                                                   {port, 8080},
                                                   {max_connections, 100}
                                                  ],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _Opts) ->
    case cowboy_req:qs_val(<<"code">>, Req) of
        {undefined, Req2} ->
            case cowboy_req:qs_val(<<"q">>, Req2) of
                {undefined, Req3} ->
                    {ok, Req3, #state{}};
                {Text, Req3} ->
                    self() ! {send, Text},
                    {ok, Req3, #state{}}
            end;
        {Code, Req2} ->
            IntegerCode = list_to_integer(binary_to_list(Code)),
            io:format("Shuting down on init using '~p' status code~n", [IntegerCode]),
            {ok, Req3} = cowboy_req:reply(IntegerCode, Req2),
            {shutdown, Req3}
    end.

websocket_handle(Frame, Req, State) ->
    io:format("Received frame~n"),
    {reply, Frame, Req, State}.

websocket_info({send, Text}, Req, State) ->
    io:format("Sent frame~n"),
    {reply, {text, Text}, Req, State};

websocket_info(_Msg, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    io:format("Server terminating with reason ~p~n", [Reason]),
    ok.

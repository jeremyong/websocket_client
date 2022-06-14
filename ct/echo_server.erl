-module(echo_server).

-behaviour(cowboy_websocket).

-export([
         start/0
        ]).

-export([
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

-record(state, {
    req :: cowboy_req:req(),
    query_params :: proplists:proplist()
}).

start() ->
    io:format("Starting echo server.~n"),
    Dispatch = cowboy_router:compile([{'_', [
                                             {"/hello", ?MODULE, []},
                                             {'_', ?MODULE, []}
                                            ]}]),
    {ok, _} = cowboy:start_clear(echo_listener,[
                                                   {port, 8080},
                                                   {max_connections, 100}
                                                  ],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

init(Req, _Opts) ->
    QueryParams = cowboy_req:parse_qs(Req),
    State = #state{req = Req, query_params = QueryParams},
    Result = case proplists:lookup(<<"code">>, QueryParams) of
        none ->
            {cowboy_websocket, Req, State};
        {<<"code">>, Code} ->
            IntegerCode = list_to_integer(binary_to_list(Code)),
            io:format("Shuting down on init using '~p' status code~n", [IntegerCode]),
            {ok, cowboy_req:reply(IntegerCode, Req), State}
    end,
    Result.

websocket_init(#state{query_params = QueryParams} = State) ->
    case proplists:lookup(<<"q">>, QueryParams) of
        none ->
            {ok, State};
        {<<"q">>, Text} ->
            {[{text, Text}], State}
    end.

websocket_handle(Frame, State) ->
    io:format("Received frame~n"),
    {reply, Frame, State}.

websocket_info(_Msg, State) ->
    {ok, State}.

terminate(_State, _PartialReq, Reason) ->
    io:format("Server terminating with reason ~p~n", [Reason]),
    ok.

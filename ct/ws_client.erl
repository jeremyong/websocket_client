-module(ws_client).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         start_link/1,
         send_text/2,
         send_binary/2,
         send_ping/2,
         recv/2,
         recv/1,
         stop/1
        ]).

-export([
         init/2,
         terminate/2,
         handle_info/2,
         websocket_init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {
          buffer = [] :: list(),
          waiting = undefined :: undefined | pid()
         }).

start_link() ->
    start_link("ws://localhost:8080").

start_link(Url) ->
    websocket_client:start_link(Url, ?MODULE, []).

stop(Pid) ->
    Pid ! stop.

send_text(Pid, Msg) ->
    websocket_client:cast(Pid, {text, Msg}).

send_binary(Pid, Msg) ->
    websocket_client:cast(Pid, {binary, Msg}).

send_ping(Pid, Msg) ->
    websocket_client:cast(Pid, {ping, Msg}).

recv(Pid) ->
    recv(Pid, 5000).

recv(Pid, Timeout) ->
    Pid ! {recv, self()},
    receive
        M -> M
    after
        Timeout -> error
    end.

init(Args, WSUri) ->
    {ok, Args, WSUri}.

terminate(_Reason, _WSUri) ->
    ok.

handle_info(Msg, WSUri) ->
    {noreply, WSUri}.

websocket_init(_, _WSReq) ->
    {ok, #state{}}.

websocket_handle(Frame, _, State = #state{waiting = undefined, buffer = Buffer}) ->
    io:format("Client received frame~n"),
    {ok, State#state{buffer = [Frame|Buffer]}};
websocket_handle(Frame, _, State = #state{waiting = From}) ->
    io:format("Client received frame~n"),
    From ! Frame,
    {ok, State#state{waiting = undefined}}.

websocket_info({send_text, Text}, WSReq, State) ->
    websocket_client:send({text, Text}, WSReq),
    {ok, State};
websocket_info({recv, From}, _, State = #state{buffer = []}) ->
    {ok, State#state{waiting = From}};
websocket_info({recv, From}, _, State = #state{buffer = [Top|Rest]}) ->
    From ! Top,
    {ok, State#state{buffer = Rest}};
websocket_info(stop, _, State) ->
    {close, <<>>, State}.

websocket_terminate(Close, _, State) ->
    io:format("Websocket closed with frame ~p and state ~p", [Close, State]),
    ok.

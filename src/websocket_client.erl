%% @author Jeremy Ong
%% @doc Erlang websocket client
-module(websocket_client).

-export([
         start_link/3,
         start_link/4,
         cast/2,
         send/2
        ]).
-export([parse_uri/1]).

-export([handler_init/4]).

-type opt() :: {extra_headers, [{string() | binary(), string() | binary()}]}.
-type opts() :: [opt()].

-record(ws_uri, {
        protocol :: atom(),
        host :: string(),
        port :: integer(),
        path :: string(),
        opts :: opts()
    }).


%% @doc Start the websocket client
-spec start_link(URL :: string(), Handler :: module(), HandlerArgs :: list()) ->
                        {ok, pid()} | {error, term()}.
start_link(URL, Handler, HandlerArgs) ->
    start_link(URL, Handler, HandlerArgs, []).

start_link(URL, Handler, HandlerArgs, Opts) when is_list(Opts) ->
    case parse_uri(URL) of
        {ok, {Protocol, _, Host, Port, Path, Query}} ->
            WsUri = #ws_uri{
                protocol = Protocol,
                host = Host,
                port = Port,
                path = Path ++ Query,
                opts = Opts
            },

            Ref = erlang:make_ref(),
            Pid = spawn_link(?MODULE, handler_init, [
                    {self(), Ref}, Handler, HandlerArgs, WsUri
                ]),
            receive
                {Ref, Reply} -> Reply;
                {'EXIT', Pid, {error,  Reason}} -> {error, {'EXIT', Reason}};
                {'EXIT', Pid, Reason} -> {error, {'EXIT', Reason}}
            end;
        {error, _} = Error ->
            Error
    end.


-spec parse_uri(URL :: string()) ->
        {ok, any()} | {error, any()}.
parse_uri(URL) ->
    http_uri:parse(URL, [{scheme_defaults, [{ws,80},{wss,443}]}]).


%% Send a frame asynchronously
-spec cast(Client :: pid(), Frame :: websocket_req:frame()) ->
                  ok.
cast(Client, Frame) ->
    Client ! {cast, Frame},
    ok.

init_ack({Pid, Ref}, Reply) -> Pid ! {Ref, Reply}.


-spec handler_init(From :: {pid(), reference()}, Handler :: module(),
                   Args :: list(), Opts :: opts()) ->
                       no_return().
handler_init(From, Handler, Args, WsUri) ->
    case Handler:init(Args, WsUri) of
        {ok, NewArgs, NewWsUri} ->
            wc_client_init(From, Handler, NewArgs, NewWsUri);
        {ok, NewArgs, NewWsUri, Delay} ->
            init_ack(From, {ok, self()}),
            handler_delay_loop(Delay, Handler, NewArgs, NewWsUri)
    end.

handler_delay_loop(infinity, Handler, HandlerArgs, WsUri) ->
    handler_loop(Handler, HandlerArgs, WsUri);
handler_delay_loop(Delay, Handler, HandlerArgs, WsUri) when is_integer(Delay) ->
    receive after Delay ->
        ws_client_init_async(Handler, HandlerArgs, WsUri)
    end.

handler_loop(Handler, HandlerArgs, WsUri) ->
    receive
        Msg -> handler_msg(Msg, Handler, HandlerArgs, WsUri)
    end.

handler_msg(Msg, Handler, HandlerArgs, WsUri) ->
    case Handler:handle_info(Msg, WsUri) of
        {noreply, NewWsUri} -> handler_loop(Handler, HandlerArgs, NewWsUri);
        {connect, NewWsUri} ->
            ws_client_init_async(Handler, HandlerArgs, NewWsUri);
        {connect, NewHandlerArgs, NewWsUri} ->
            ws_client_init_async(Handler, NewHandlerArgs, NewWsUri);
        {stop, Reason, NewWsUri} ->
            Handler:terminate(Reason, NewWsUri)
    end.


wc_client_init(From, Handler, HandlerArgs, WsUri) ->
    case ws_connection_init(Handler, HandlerArgs, WsUri) of
        {ok, WSReq, HandlerState, Buffer} ->
            init_ack(From, {ok, self()}),
            ws_loop_init(WSReq, HandlerState, Buffer);
        {error, _} = Error ->
            init_ack(From, Error)
    end.

ws_client_init_async(Handler, HandlerArgs, WsUri) ->
    case ws_connection_init(Handler, HandlerArgs, WsUri) of
        {ok, WSReq, HandlerState, Buffer} ->
            ws_loop_init(WSReq, HandlerState, Buffer);
        {error, _} = Error ->
            Handler:terminate(Error, WsUri)
    end.

transport(wss) -> ssl;
transport(ws) -> gen_tcp.

socket_connect(wss, Host, Port) ->
    ssl:connect(Host, Port, [
            {mode, binary}, {verify, verify_none}, {active, false}, {packet, 0}
        ], 6000);
socket_connect(ws, Host, Port) ->
    gen_tcp:connect(Host, Port, [
            binary, {active, false}, {packet, 0}
        ], 6000).

socket_active(wss, Socket) -> ssl:setopts(Socket, [{active, true}]);
socket_active(ws, Socket) -> inet:setopts(Socket, [{active, true}]).

keepalive(WSReq) ->
    case websocket_req:keepalive_timer(WSReq) of
        undefined -> ok;
        OldTimer -> erlang:cancel_timer(OldTimer)
    end,
    case websocket_req:keepalive(WSReq) of
        KeepAlive when is_number(KeepAlive) ->
            Timer = erlang:send_after(KeepAlive, self(), keepalive),
            websocket_req:set([ {keepalive_timer, Timer} ], WSReq);
        _ -> WSReq
    end.


ws_connection_init(Handler, HandlerArgs, #ws_uri{
        protocol = Protocol, host = Host, port = Port, path = Path, opts = Opts
    }) ->
    case socket_connect(Protocol, Host, Port) of
        {error, _} = ConnectError ->
            ConnectError;
        {ok, Socket} ->
            WSReq0 = websocket_req:new( Protocol, Host, Port, Path, Socket, transport(Protocol), Handler),
            ExtraHeaders = proplists:get_value(extra_headers, Opts, []),
            case websocket_handshake(WSReq0, ExtraHeaders) of
                {error, HandshakeError} ->
                    {error, HandshakeError};
                {ok, Buffer} ->
                    {ok, HandlerState, WSReq1} = case Handler:websocket_init(HandlerArgs, WSReq0) of
                        {ok, HS} -> {ok, HS, WSReq0};
                        {ok, HS, KeepAlive} ->
                            {ok, HS, websocket_req:keepalive(KeepAlive, WSReq0)}
                    end,
                    {ok, WSReq1, HandlerState, Buffer}
            end
    end.

ws_loop_init(WSReq0, HandlerState, Buffer) ->
    Transport = websocket_req:transport(WSReq0),
    Protocol = websocket_req:protocol(WSReq0),
    Socket = websocket_req:socket(WSReq0),

    socket_active(Protocol, Socket), %% done
    %% Since we could have already received some data already, we simulate a Socket message.
    case Buffer of
        <<>> -> ok;
        _ -> self() ! {Transport, Socket, Buffer}
    end,

    WSReq = keepalive(WSReq0),
    websocket_loop(WSReq, HandlerState, <<>>).



%% @doc Send http upgrade request and validate handshake response challenge
-spec websocket_handshake(WSReq :: websocket_req:req(), [{string(), string()}]) -> {ok, binary()} | {error, term()}.
websocket_handshake(WSReq, ExtraHeaders) ->
    [Path, Host, Key, Transport, Socket] =
        websocket_req:get([path, host, key, transport, socket], WSReq),
    Handshake = ["GET ", Path, " HTTP/1.1\r\n"
                 "Host: ", Host, "\r\n"
                 "Connection: Upgrade\r\n"
                 "Sec-WebSocket-Version: 13\r\n"
                 "Sec-WebSocket-Key: ", Key, "\r\n"
                 "Upgrade: websocket\r\n",
                 [ [Header, ": ", Value, "\r\n"] || {Header, Value} <- ExtraHeaders],
                 "\r\n"],
    Transport:send(Socket, Handshake),
    case receive_handshake(<<>>, Transport, Socket) of
        {ok, HandshakeResponse} ->
            validate_handshake(HandshakeResponse, Key);
        {error, _} = Error ->
            Error
    end.

%% @doc Blocks and waits until handshake response data is received
-spec receive_handshake(Buffer :: binary(),
                        Transport :: module(),
                        Socket :: term()) ->
                               {ok, binary()}.
receive_handshake(Buffer, Transport, Socket) ->
    case re:run(Buffer, "\\r\\n\\r\\n") of
        {match, _} -> {ok, Buffer};
        _ ->
            case Transport:recv(Socket, 0, 6000) of
                {ok, Data} ->
                    NewBuffer = <<Buffer/binary, Data/binary>>,
                    receive_handshake(NewBuffer, Transport, Socket);
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Send frame to server
send(Frame, WSReq) ->
    [Transport, Socket] = websocket_req:get([transport, socket], WSReq),
    Transport:send(Socket, encode_frame(Frame)).

%% @doc Main loop
-spec websocket_loop(WSReq :: websocket_req:req(), HandlerState :: any(),
                     Buffer :: binary()) ->
                            ok.
websocket_loop(WSReq, HandlerState, Buffer) ->
  receive
    Message -> handle_websocket_message(WSReq, HandlerState, Buffer, Message)
  end.

handle_websocket_message(WSReq, HandlerState, Buffer, Message) ->
    [Handler, Remaining, Socket] =
        websocket_req:get([handler, remaining, socket], WSReq),
    case Message of
        keepalive ->
            case websocket_req:get([keepalive_timer], WSReq) of
              [undefined] -> ok;
              [OldTimer] -> erlang:cancel_timer(OldTimer)
            end,
            ok = send({ping, <<>>}, WSReq),
            NewWSReq = keepalive(WSReq),
            websocket_loop(NewWSReq, HandlerState, Buffer);
        {cast, Frame} ->
            ok = send(Frame, WSReq),
            websocket_loop(WSReq, HandlerState, Buffer);
        {Closed, Socket} ->
            websocket_close(WSReq, HandlerState, {remote, Closed});
        {_TransportType, Socket, Data} ->
            case Remaining of
                undefined ->
                    retrieve_frame(WSReq, HandlerState,
                                   << Buffer/binary, Data/binary >>);
                _ ->
                    retrieve_frame(WSReq, HandlerState,
                                   websocket_req:opcode(WSReq), Remaining, Data, Buffer)
            end;
        Msg ->
            Handler = websocket_req:handler(WSReq),
            try Handler:websocket_info(Msg, WSReq, HandlerState) of
              HandlerResponse ->
                handle_response(WSReq, HandlerResponse, Buffer)
            catch Class:Reason ->
              error_logger:error_msg(
                "** Websocket client ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n"
                "** Last message was ~p~n"
                "** Handler state was ~p~n"
                "** Stacktrace: ~p~n~n",
                [Handler, websocket_info, 3, Class, Reason, Msg, HandlerState,
                  erlang:get_stacktrace()]),
              websocket_close(WSReq, HandlerState, Reason)
            end
    end.

-spec websocket_close(WSReq :: websocket_req:req(),
                      HandlerState :: any(),
                      Reason :: tuple()) -> ok.
websocket_close(WSReq, HandlerState, Reason) ->
    Handler = websocket_req:handler(WSReq),
    try Handler:websocket_terminate(Reason, WSReq, HandlerState)
    catch Class:Reason2 ->
      error_logger:error_msg(
        "** Websocket handler ~p terminating in ~p/~p~n"
        "   for the reason ~p:~p~n"
        "** Handler state was ~p~n"
        "** Stacktrace: ~p~n~n",
        [Handler, websocket_terminate, 3, Class, Reason2, HandlerState,
          erlang:get_stacktrace()])
    end.


%% @doc Validate handshake response challenge
-spec validate_handshake(HandshakeResponse :: binary(), Key :: binary()) -> {ok, binary()} | {error, term()}.
validate_handshake(HandshakeResponse, Key) ->
    Challenge = base64:encode(
                  crypto:hash(sha, << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    %% Consume the response...
    {ok, Status, Header, Buffer} = consume_response(HandshakeResponse),
    {_Version, Code, Message} = Status,
    case Code of
        % 101 means Switching Protocol
        101 ->
            %% ...and make sure the challenge is valid.
            Challenge = proplists:get_value(<<"Sec-Websocket-Accept">>, Header),
            {ok, Buffer};
        _ -> {error, {Code, Message}}
    end.

%% @doc Consumes the HTTP response and extracts status, header and the body.
consume_response(Response) ->
    {ok, {http_response, Version, Code, Message}, Header} = erlang:decode_packet(http_bin, Response, []),
    consume_response({Version, Code, Message}, Header, []).

consume_response(Status, Response, HeaderAcc) ->
    case erlang:decode_packet(httph_bin, Response, []) of
        {ok, {http_header, _Length, Field, _Reserved, Value}, Rest} ->
            consume_response(Status, Rest, [{Field, Value} | HeaderAcc]);

        {ok, http_eoh, Body} ->
            {ok, Status, HeaderAcc, Body}
    end.

%% @doc Start or continue continuation payload with length less than 126 bytes
retrieve_frame(WSReq, HandlerWSReq,
               << 0:4, Opcode:4, 0:1, Len:7, Rest/bits >>)
  when Len < 126 ->
    WSReq1 = set_continuation_if_empty(WSReq, Opcode),
    WSReq2 = websocket_req:fin(0, WSReq1),
    retrieve_frame(WSReq2, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Start or continue continuation payload with length a 2 byte int
retrieve_frame(WSReq, HandlerWSReq,
               << 0:4, Opcode:4, 0:1, 126:7, Len:16, Rest/bits >>)
  when Len > 125, Opcode < 8 ->
    WSReq1 = set_continuation_if_empty(WSReq, Opcode),
    WSReq2 = websocket_req:fin(0, WSReq1),
    retrieve_frame(WSReq2, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Start or continue continuation payload with length a 64 bit int
retrieve_frame(WSReq, HandlerWSReq,
               << 0:4, Opcode:4, 0:1, 127:7, 0:1, Len:63, Rest/bits >>)
  when Len > 16#ffff, Opcode < 8 ->
    WSReq1 = set_continuation_if_empty(WSReq, Opcode),
    WSReq2 = websocket_req:fin(0, WSReq1),
    retrieve_frame(WSReq2, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Length is less 126 bytes
retrieve_frame(WSReq, HandlerWSReq,
               << 1:1, 0:3, Opcode:4, 0:1, Len:7, Rest/bits >>)
  when Len < 126 ->
    WSReq1 = websocket_req:fin(1, WSReq),
    retrieve_frame(WSReq1, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Length is a 2 byte integer
retrieve_frame(WSReq, HandlerWSReq,
               << 1:1, 0:3, Opcode:4, 0:1, 126:7, Len:16, Rest/bits >>)
  when Len > 125, Opcode < 8 ->
    WSReq1 = websocket_req:fin(1, WSReq),
    retrieve_frame(WSReq1, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Length is a 64 bit integer
retrieve_frame(WSReq, HandlerWSReq,
               << 1:1, 0:3, Opcode:4, 0:1, 127:7, 0:1, Len:63, Rest/bits >>)
  when Len > 16#ffff, Opcode < 8 ->
    WSReq1 = websocket_req:fin(1, WSReq),
    retrieve_frame(WSReq1, HandlerWSReq, Opcode, Len, Rest, <<>>);
%% @doc Need more data to read length properly
retrieve_frame(WSReq, HandlerWSReq, Data) ->
    websocket_loop(WSReq, HandlerWSReq, Data).

%% @doc Length known and still missing data
retrieve_frame(WSReq, HandlerWSReq, Opcode, Len, Data, Buffer)
  when byte_size(Data) < Len ->
    Remaining = Len - byte_size(Data),
    WSReq1 = websocket_req:remaining(Remaining, WSReq),
    WSReq2  = websocket_req:opcode(Opcode, WSReq1),
    websocket_loop(WSReq2, HandlerWSReq, << Buffer/bits, Data/bits >>);
%% @doc Length known and remaining data is appended to the buffer
retrieve_frame(WSReq, HandlerState, Opcode, Len, Data, Buffer) ->
    [Handler, Continuation, ContinuationOpcode] =
        websocket_req:get([handler, continuation, continuation_opcode], WSReq),
    Fin = websocket_req:fin(WSReq),
    << Payload:Len/binary, Rest/bits >> = Data,
    FullPayload = << Buffer/binary, Payload/binary >>,
    OpcodeName = websocket_req:opcode_to_name(Opcode),
    case OpcodeName of
        ping ->
            %% If a ping is received, send a pong automatically
            ok = send({pong, FullPayload}, WSReq);
        _ ->
            ok
    end,
    case OpcodeName of
        close when byte_size(FullPayload) >= 2 ->
            << CodeBin:2/binary, ClosePayload/binary >> = FullPayload,
            Code = binary:decode_unsigned(CodeBin),
            Reason = case Code of
                         1000 -> {remote, ClosePayload};
                         1002 -> {error, badframe, ClosePayload};
                         1007 -> {error, badencoding, ClosePayload};
                         1011 -> {error, handler, ClosePayload};
                         _ -> {remote, Code, ClosePayload}
                     end,
            websocket_close(WSReq, HandlerState, Reason);
        close ->
            websocket_close(WSReq, HandlerState, {remote, <<>>});
        %% Non-control continuation frame
        _ when Opcode < 8, Continuation =/= undefined, Fin == 0 ->
            %% Append to previously existing continuation payloads and continue
            Continuation1 = << Continuation/binary, FullPayload/binary >>,
            WSReq1 = websocket_req:continuation(Continuation1, WSReq),
            retrieve_frame(WSReq1, HandlerState, Rest);
        %% Terminate continuation frame sequence with non-control frame
        _ when Opcode < 8, Continuation =/= undefined, Fin == 1 ->
            DefragPayload = << Continuation/binary, FullPayload/binary >>,
            WSReq1 = websocket_req:continuation(undefined, WSReq),
            WSReq2 = websocket_req:continuation_opcode(undefined, WSReq1),
            ContinuationOpcodeName = websocket_req:opcode_to_name(ContinuationOpcode),
            try Handler:websocket_handle(
                                {ContinuationOpcodeName, DefragPayload},
                                WSReq2, HandlerState) of
              HandlerResponse ->
                handle_response(websocket_req:remaining(undefined, WSReq1),
                                HandlerResponse, Rest)
            catch Class:Reason ->
              error_logger:error_msg(
                "** Websocket client ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n"
                "** Websocket message was ~p~n"
                "** Handler state was ~p~n"
                "** Stacktrace: ~p~n~n",
                [Handler, websocket_handle, 3, Class, Reason, {ContinuationOpcodeName, DefragPayload}, HandlerState,
                  erlang:get_stacktrace()]),
              websocket_close(WSReq, HandlerState, Reason)
            end;
        _ ->
            try Handler:websocket_handle(
                                {OpcodeName, FullPayload},
                                WSReq, HandlerState) of
              HandlerResponse ->
                handle_response(websocket_req:remaining(undefined, WSReq),
                                HandlerResponse, Rest)
            catch Class:Reason ->
              error_logger:error_msg(
                "** Websocket client ~p terminating in ~p/~p~n"
                "   for the reason ~p:~p~n"
                "** Handler state was ~p~n"
                "** Stacktrace: ~p~n~n",
                [Handler, websocket_handle, 3, Class, Reason, HandlerState,
                  erlang:get_stacktrace()]),
              websocket_close(WSReq, HandlerState, Reason)
            end
    end.

%% @doc Handles return values from the callback module
handle_response(WSReq, {reply, Frame, HandlerState}, Buffer) ->
    case send(Frame, WSReq) of
        ok ->
           %% we can still have more messages in buffer
           case websocket_req:remaining(WSReq) of
               %% buffer should not contain uncomplete messages
               undefined -> retrieve_frame(WSReq, HandlerState, Buffer);
               %% buffer contain uncomplete message that shouldnt be parsed
               _ -> websocket_loop(WSReq, HandlerState, Buffer)
           end;
        Reason -> websocket_close(WSReq, HandlerState, Reason)
    end;
handle_response(WSReq, {ok, HandlerState}, Buffer) ->
    %% we can still have more messages in buffer
    case websocket_req:remaining(WSReq) of
        %% buffer should not contain uncomplete messages
        undefined -> retrieve_frame(WSReq, HandlerState, Buffer);
        %% buffer contain uncomplete message that shouldnt be parsed
        _ -> websocket_loop(WSReq, HandlerState, Buffer)
    end;

handle_response(WSReq, {close, Payload, HandlerState}, _) when is_binary(Payload) ->
    send({close, Payload}, WSReq),
    websocket_close(WSReq, HandlerState, {normal, Payload});

handle_response(WSReq, {close, Error, HandlerState}, _) ->
    websocket_close(WSReq, HandlerState, Error).

%% @doc Encodes the data with a header (including a masking key) and
%% masks the data
-spec encode_frame(websocket_req:frame()) ->
                          binary().
encode_frame({Type, Payload}) ->
    Opcode = websocket_req:name_to_opcode(Type),
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    MaskingKeyBin = crypto:rand_bytes(4),
    << MaskingKey:32 >> = MaskingKeyBin,
    Header = << 1:1, 0:3, Opcode:4, 1:1, BinLen/bits, MaskingKeyBin/bits >>,
    MaskedPayload = mask_payload(MaskingKey, Payload),
    << Header/binary, MaskedPayload/binary >>;
encode_frame(Type) when is_atom(Type) ->
    encode_frame({Type, <<>>}).

%% @doc The payload is masked using a masking key byte by byte.
%% Can do it in 4 byte chunks to save time until there is left than 4 bytes left
mask_payload(MaskingKey, Payload) ->
    mask_payload(MaskingKey, Payload, <<>>).
mask_payload(_, <<>>, Acc) ->
    Acc;
mask_payload(MaskingKey, << D:32, Rest/bits >>, Acc) ->
    T = D bxor MaskingKey,
    mask_payload(MaskingKey, Rest, << Acc/binary, T:32 >>);
mask_payload(MaskingKey, << D:24 >>, Acc) ->
    << MaskingKeyPart:24, _:8 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:24 >>;
mask_payload(MaskingKey, << D:16 >>, Acc) ->
    << MaskingKeyPart:16, _:16 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:16 >>;
mask_payload(MaskingKey, << D:8 >>, Acc) ->
    << MaskingKeyPart:8, _:24 >> = << MaskingKey:32 >>,
    T = D bxor MaskingKeyPart,
    << Acc/binary, T:8 >>.

%% @doc Encode the payload length as binary in a variable number of bits.
%% See RFC Doc for more details
payload_length_to_binary(Len) when Len =<125 ->
    << Len:7 >>;
payload_length_to_binary(Len) when Len =< 16#ffff ->
    << 126:7, Len:16 >>;
payload_length_to_binary(Len) when Len =< 16#7fffffffffffffff ->
    << 127:7, Len:64 >>.

%% @doc If this is the first continuation frame, set the opcode and initialize
%% continuation to an empty binary. Otherwise, return the request object untouched.
-spec set_continuation_if_empty(WSReq :: websocket_req:req(),
                                Opcode :: websocket_req:opcode()) ->
                                       websocket_req:req().
set_continuation_if_empty(WSReq, Opcode) ->
    case websocket_req:continuation(WSReq) of
        undefined ->
            WSReq1 = websocket_req:continuation_opcode(Opcode, WSReq),
            websocket_req:continuation(<<>>, WSReq1);
        _ ->
            WSReq
    end.

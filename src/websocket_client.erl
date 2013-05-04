%% @author Jeremy Ong
%% @doc Erlang websocket client
-module(websocket_client).

-export([
         start_link/3,
         async_send/2,
         send/2,
         send/3
        ]).

-export([ws_client_init/6]).

%% @doc Start the websocket client
-spec start_link(URL :: string(), Handler :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(URL, Handler, Args) ->
  case http_uri:parse(URL, [{scheme_defaults, [{ws,80},{wss,443}]}]) of
    {ok, {Protocol, _, Host, Port, Path, Query}} ->
      proc_lib:start_link(?MODULE, ws_client_init,
               [Handler, Protocol, Host, Port, Path ++ Query, Args]);
    {error, _} = Error ->
      Error
  end.

%% Send a frame asynchronously
-spec async_send(Client :: pid(), Frame :: websocket_req:frame()) ->
    ok.
async_send(Client, Frame) ->
    Client ! {async_send, Frame},
    ok.

-spec send(Client :: pid(), Frame :: websocket_req:frame()) ->
    ok | {error, term()}.
send(Client, Frame) ->
    send(Client, Frame, 5000).

-spec send(Client :: pid(), Frame :: websocket_req:frame(), Timeout :: integer()) ->
    ok | {error, term()}.
send(Client, Frame, Timeout) ->
    Client ! {send, self(), Ref = erlang:make_ref(), Frame},
    receive
        {reply, Ref, Reply} ->
            Reply
    after Timeout ->
        {error, timeout}
    end.

    

%% @doc Create socket, execute handshake, and enter loop
-spec ws_client_init(Handler :: module(), Protocol :: websocket_req:protocol(),
                     Host :: string(), Port :: inet:port_number(), Path :: string(),
                     Args :: list()) ->
    no_return().
ws_client_init(Handler, Protocol, Host, Port, Path, Args) ->
    Transport = case Protocol of
                    wss ->
                        ssl;
                    ws ->
                        gen_tcp
                end,
    SockReply = case Transport of
                       ssl ->
                           ssl:connect(Host, Port,
                                       [{mode, binary},
                                        {verify, verify_none},
                                        {active, false},
                                        {packet, 0}
                                       ], 6000);
                       gen_tcp ->
                           gen_tcp:connect(Host, Port,
                                           [binary,
                                            {active, false},
                                            {packet, 0}
                                           ], 6000)
                   end,
    {ok, Socket} = case SockReply of
      {ok, Sock} -> {ok, Sock};
      {error, _} = ConnectError ->
        proc_lib:init_ack(ConnectError),
        exit(normal)
    end,
    proc_lib:init_ack({ok, self()}),
    WSReq = websocket_req:new(
      Protocol,
      Host,
      Port,
      Path,
      Socket,
      Transport,
      Handler,
      generate_ws_key()
    ),
    ok = websocket_handshake(WSReq),
    case Socket of
        {sslsocket, _, _} ->
            ssl:setopts(Socket, [{active, true}]);
        _ ->
            inet:setopts(Socket, [{active, true}])
    end,
    {ok, HandlerState, KeepAlive} = case Handler:init(Args, WSReq) of
                                        {ok, HS} ->
                                            {ok, HS, infinity};
                                        {ok, HS, KA} ->
                                            {ok, HS, KA}
                                    end,
    case KeepAlive of
        infinity ->
            ok;
        _ ->
            erlang:send_after(KeepAlive, self(), keepalive)
    end,
    websocket_loop(websocket_req:keepalive(KeepAlive, WSReq), HandlerState, <<>>).

%% @doc Send http upgrade request and validate handshake response challenge
-spec websocket_handshake(WSReq :: websocket_req:req()) ->
    ok.
websocket_handshake(WSReq) ->
    [Protocol, Path, Host, Key, Transport, Socket] =
        websocket_req:get([protocol, path, host, key, transport, socket], WSReq),
    Handshake = [<<"GET ">>, Path,
                 <<" HTTP/1.1"
                   "\r\nHost: ">>, Host,
                 <<"\r\nUpgrade: WebSocket"
                   "\r\nConnection: Upgrade"
                   "\r\nSec-WebSocket-Key: ">>, Key,
                 <<"\r\nOrigin: ">>, atom_to_binary(Protocol, utf8), <<"://">>, Host,
                 <<"\r\nSec-WebSocket-Protocol: "
                   "\r\nSec-WebSocket-Version: 13"
                   "\r\n\r\n">>],
    Transport = websocket_req:transport(WSReq),
    Socket =    websocket_req:socket(WSReq),
    Transport:send(Socket, Handshake),
    {ok, HandshakeResponse} = receive_handshake(<<>>, Transport, Socket),
    validate_handshake(HandshakeResponse, Key),
    ok.

%% @doc Blocks and waits until handshake response data is received
-spec receive_handshake(Buffer :: binary(),
                        Transport :: module(),
                        Socket :: term()) ->
    {ok, binary()}.
receive_handshake(Buffer, Transport, Socket) ->
    case re:run(Buffer, ".*\\r\\n\\r\\n") of
        {match, _} ->
            {ok, Buffer};
        _ ->
            {ok, Data} = Transport:recv(Socket, 0, 6000),
            receive_handshake(<< Buffer/binary, Data/binary >>,
                              Transport, Socket)
    end.

%% @doc Main loop
-spec websocket_loop(WSReq :: websocket_req:req(), HandlerState :: any(),
                     Buffer :: binary()) ->
    ok.
websocket_loop(WSReq, HandlerState, Buffer) ->
    [Handler, Remaining, Socket, Transport] =
        websocket_req:get([handler, remaining, socket, transport], WSReq),
    receive
        keepalive ->
            ok = Transport:send(Socket, encode_frame({ping, <<>>})),
            erlang:send_after(websocket_req:keepalive(WSReq), self(), keepalive),
            websocket_loop(WSReq, HandlerState, Buffer);
        {async_send, Frame} ->
            ok = Transport:send(Socket, encode_frame(Frame)),
            websocket_loop(WSReq, HandlerState, Buffer);
        {send, From, Ref, Frame} ->
            Reply = Transport:send(Socket, encode_frame(Frame)),
            From ! {reply, Ref, Reply},
            websocket_loop(WSReq, HandlerState, Buffer);
        {_Closed, Socket} ->
            websocket_close(WSReq, HandlerState, {remote, closed});
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
            HandlerResponse = Handler:websocket_info(Msg, WSReq, HandlerState),
            handle_response(WSReq, HandlerResponse, Buffer)
    end,
    ok.

-spec websocket_close(WSReq :: websocket_req:req(),
                      HandlerState :: any(),
                      Reason :: tuple()) -> ok.
websocket_close(WSReq, HandlerState, Reason) ->
    Handler = websocket_req:handler(WSReq),
    Handler:websocket_terminate(Reason, WSReq, HandlerState).

%% @doc Key sent in initial handshake
-spec generate_ws_key() ->
    binary().
generate_ws_key() ->
    base64:encode(crypto:rand_bytes(16)).

%% @doc Validate handshake response challenge
-spec validate_handshake(HandshakeResponse :: binary(), Key :: binary()) ->
    ok.
validate_handshake(HandshakeResponse, Key) ->
    Challenge = base64:encode(
                  crypto:sha(<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    {match, [Challenge]} = re:run(
                             HandshakeResponse,
                             ".*[s|S]ec-[w|W]eb[s|S]ocket-[a|A]ccept: (.*)\\r\\n.*",
                             [{capture, [1], binary}]),
    ok.

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
            [Transport, Socket] = websocket_req:get([transport, socket], WSReq),
            ok = Transport:send(Socket, encode_frame({pong, FullPayload}));
        _ ->
            ok
    end,
    case OpcodeName of
        close when byte_size(FullPayload) >= 2 ->
            << CodeBin:2/binary, ClosePayload/binary >> = FullPayload,
            Code = binary:decode_unsigned(CodeBin),
            Reason = case Code of
                         1000 -> {normal, ClosePayload};
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
            HandlerResponse = Handler:websocket_handle(
                                {ContinuationOpcodeName, DefragPayload},
                                WSReq2, HandlerState),
            handle_response(websocket_req:remaining(undefined, WSReq1),
                            HandlerResponse, Rest);
        _ ->
            HandlerResponse = Handler:websocket_handle(
                                {OpcodeName, FullPayload},
                                WSReq, HandlerState),
            handle_response(websocket_req:remaining(undefined, WSReq),
                            HandlerResponse, Rest)
    end.

%% @doc Handles return values from the callback module
handle_response(WSReq, {reply, Frame, HandlerState}, Buffer) ->
    [Socket, Transport] = websocket_req:get([socket, transport], WSReq),
    ok = Transport:send(Socket, encode_frame(Frame)),
    retrieve_frame(WSReq, HandlerState, Buffer);
handle_response(WSReq, {ok, HandlerState}, Buffer) ->
    retrieve_frame(WSReq, HandlerState, Buffer);
handle_response(WSReq, {close, Payload, HandlerState}, _) ->
    [Socket, Transport] = websocket_req:get([socket, transport], WSReq),
    ok = Transport:send(Socket, encode_frame({close, Payload})),
    websocket_close(WSReq, HandlerState, {normal, Payload}).

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

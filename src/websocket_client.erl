%% @author Jeremy Ong
%% @doc Erlang websocket client
-module(websocket_client).

-include("websocket_client.hrl").
-export([
         start_link/3,
         cast/2
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
-spec cast(Client :: pid(), Frame :: frame()) ->
    ok.
cast(Client, Frame) ->
    Client ! {cast, Frame},
    ok.

%% @doc Create socket, execute handshake, and enter loop
-spec ws_client_init(Handler :: module(), Protocol :: protocol(),
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
    ConnState = #websocket_req{
      protocol = Protocol,
      host = Host,
      port = Port,
      path = Path,
      transport = Transport,
      handler = Handler,
      key = generate_ws_key(),
      socket = Socket
     },
    ok = websocket_handshake(ConnState),
    case Socket of
        {sslsocket, _, _} ->
            ssl:setopts(Socket, [{active, true}]);
        _ ->
            inet:setopts(Socket, [{active, true}])
    end,
    {ok, HandlerState, KeepAlive} = case Handler:init(Args, ConnState) of
                                        {ok, HS} ->
                                            {ok, HS, 45000};
                                        {ok, HS, KA} ->
                                            {ok, HS, KA}
                                    end,
    erlang:send_after(KeepAlive, self(), keepalive),
    websocket_loop(ConnState#websocket_req{keepalive = KeepAlive}, HandlerState, <<>>).

%% @doc Send http upgrade request and validate handshake response challenge
-spec websocket_handshake(ConnState :: tuple()) ->
    ok.
websocket_handshake(ConnState = #websocket_req{protocol = Protocol, path = Path, host = Host, key = Key}) ->
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
    Transport = ConnState#websocket_req.transport,
    Socket = ConnState#websocket_req.socket,
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
-spec websocket_loop(ConnState :: tuple(), HandlerState :: any(),
                     Buffer :: binary()) ->
    ok.
websocket_loop(ConnState = #websocket_req{handler = Handler, remaining = Remaining,
                              socket = Socket, transport = Transport},
               HandlerState, Buffer) ->
    receive
        keepalive ->
            ok = Transport:send(Socket, encode_frame({ping, <<>>})),
            erlang:send_after(ConnState#websocket_req.keepalive, self(), keepalive),
            websocket_loop(ConnState, HandlerState, Buffer);
        {cast, Frame} ->
            ok = Transport:send(Socket, encode_frame(Frame)),
            websocket_loop(ConnState, HandlerState, Buffer);
        {_Closed, Socket} ->
            Handler:websocket_terminate({close, 0, <<>>}, ConnState, HandlerState);
        {_TransportType, Socket, Data} ->
            case Remaining of
                undefined ->
                    retrieve_frame(ConnState, HandlerState,
                                   << Buffer/binary, Data/binary >>);
                _ ->
                    retrieve_frame(ConnState, HandlerState,
                                   ConnState#websocket_req.opcode, Remaining, Data, Buffer)
            end;
        Msg ->
            Handler = ConnState#websocket_req.handler,
            HandlerResponse = Handler:websocket_info(Msg, ConnState, HandlerState),
            handle_response(ConnState, HandlerResponse, Buffer)
    end,
    ok.

%% @doc Key sent in initial handshake
-spec generate_ws_key() ->
    binary().
generate_ws_key() ->
    base64:encode(crypto:rand_bytes(16)).

%% @doc Validate handshake response challenge
-spec validate_handshake(HandshakeResponse :: binary(), Key :: binary()) ->
    ok.
validate_handshake(HandshakeResponse, Key) ->
    Challenge = base64:encode(crypto:sha(
                                << Key/binary,
                                 "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>
                               )),
    {match, [Challenge]} = re:run(
                             HandshakeResponse,
                             ".*[s|S]ec-[w|W]eb[s|S]ocket-[a|A]ccept: (.*)\\r\\n.*",
                             [{capture, [1], binary}]),
    ok.

%% @doc Mapping from opcode to opcode name
-spec websocket_opcode_to_name(opcode()) ->
    atom().
websocket_opcode_to_name(1) -> text;
websocket_opcode_to_name(2) -> binary;
websocket_opcode_to_name(8) -> close;
websocket_opcode_to_name(9) -> ping;
websocket_opcode_to_name(10) -> pong.

%% @doc Mapping from opcode to opcode name
-spec websocket_name_to_opcode(atom()) ->
    opcode().
websocket_name_to_opcode(text) -> 1;
websocket_name_to_opcode(binary) -> 2;
websocket_name_to_opcode(close) -> 8;
websocket_name_to_opcode(ping) -> 9;
websocket_name_to_opcode(pong) -> 10.



%% @doc Length is less 126 bytes
retrieve_frame(State, HandlerState,
               << 1:1, 0:3, Opcode:4, 0:1, Len:7, Rest/bits >>)
  when Len < 126 ->
    retrieve_frame(State, HandlerState, Opcode, Len, Rest, <<>>);
%% @doc Length is a 2 byte integer
retrieve_frame(State, HandlerState,
               << 1:1, 0:3, Opcode:4, 0:1, 126:7, Len:16, Rest/bits >>)
  when Len > 125, Opcode < 8 ->
    retrieve_frame(State, HandlerState, Opcode, Len, Rest, <<>>);
%% @doc Length is a 64 bit integer
retrieve_frame(State, HandlerState,
               << 1:1, 0:3, Opcode:4, 0:1, 127:7, 0:1, Len:63, Rest/bits >>)
  when Len > 16#ffff, Opcode < 8 ->
    retrieve_frame(State, HandlerState, Opcode, Len, Rest, <<>>);
%% @doc Need more data to read length properly
retrieve_frame(State, HandlerState, Data) ->
    websocket_loop(State, HandlerState, Data).

%% @doc Length known and still missing data
retrieve_frame(State, HandlerState, Opcode, Len, Data, Buffer)
  when byte_size(Data) < Len ->
    Remaining = Len - byte_size(Data),
    websocket_loop(State#websocket_req{remaining = Remaining, opcode = Opcode},
                   HandlerState, << Buffer/bits, Data/bits >>);
%% @doc Length known and remaining data is appended to the buffer
retrieve_frame(ConnState = #websocket_req{
                 handler = Handler, transport = Transport, socket = Socket
                },
               HandlerState, Opcode, Len, Data, Buffer) ->
    << Payload:Len/binary, Rest/bits >> = Data,
    FullPayload = << Buffer/binary, Payload/binary >>,
    OpcodeName = websocket_opcode_to_name(Opcode),
    case OpcodeName of
        ping ->
            %% If a ping is received, send a pong  automatically
            ok = Transport:send(Socket, encode_frame({pong, FullPayload}));
        _ ->
            ok
    end,
    case OpcodeName of
        close when byte_size(FullPayload) >= 2 ->
            << CodeBin:2/binary, ClosePayload/binary >> = FullPayload,
            Code = binary:decode_unsigned(CodeBin),
            Handler:websocket_terminate({close, Code, ClosePayload},
                                        ConnState, HandlerState);
        close ->
            Handler:websocket_terminate({close, 0, <<>>}, 
                                        ConnState, HandlerState);
        _ ->
            HandlerResponse = Handler:websocket_handle(
                                {OpcodeName, FullPayload},
                                ConnState, HandlerState),
            handle_response(ConnState#websocket_req{remaining = undefined},
                            HandlerResponse, Rest)
    end.

%% @doc Handles return values from the callback module
handle_response(ConnState = #websocket_req{socket = Socket, transport = Transport},
                {reply, Frame, HandlerState}, Buffer) ->
    ok = Transport:send(Socket, encode_frame(Frame)),
    websocket_loop(ConnState, HandlerState, Buffer);
handle_response(ConnState = #websocket_req{socket = Socket, transport = Transport},
               {close, Payload, HandlerState}, Buffer) ->
    ok = Transport:send(Socket, encode_frame({close, Payload})),
    websocket_loop(ConnState, HandlerState, Buffer);
handle_response(ConnState, {ok, HandlerState}, Buffer) ->
    websocket_loop(ConnState, HandlerState, Buffer).

%% @doc Encodes the data with a header (including a masking key) and
%% masks the data
-spec encode_frame(frame()) ->
    binary().
encode_frame({Type, Payload}) ->
    Opcode = websocket_name_to_opcode(Type),
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

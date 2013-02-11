%% @author Jeremy Ong
%% @doc Erlang websocket client
-module(websocket_client).

-export([
         start_link/6,
         ws_client_init/6,
         encode_frame/1
        ]).

-type protocol() :: ws | wss.

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.

-type frame() :: close | ping | pong |
  {text | binary | close | ping | pong, binary()}
  | {close, 1000..4999, binary()}.

-record(state, {
          host :: binary(),
          port :: integer(),
          path ::  binary(),
          socket = undefined :: inet:socket(),
          transport = undefined :: module(),
          handler :: module(),
          key = undefined :: undefined | binary(),
          remaining = undefined :: integer(),
          opcode = undefined :: opcode()
         }).

-spec start_link(Handler :: module(), Protocol :: protocol(),
                 Host :: list(), Port :: integer(), Path :: list(),
                 Args :: list()) ->
    pid().
start_link(Handler, Protocol, Host, Port, Path, Args) ->
    spawn_link(?MODULE, ws_client_init, [Handler, Protocol, Host, Port, Path, Args]).

%% @doc Create socket, execute handshake, and enter loop
-spec ws_client_init(Handler :: module(), Protocol :: protocol(),
                     Host :: list(), Port :: integer(), Path :: list(),
                     Args :: list()) ->
    pid().
ws_client_init(Handler, Protocol, Host, Port, Path, Args) ->
    {ok, HandlerState} = Handler:init(Args),
    Transport = case Protocol of
                    wss ->
                        ssl;
                    ws ->
                        gen_tcp
                end,
    {ok, Socket} = case Transport of
                       ssl ->
                           ssl:connect(Host, Port,
                                       [{mode, binary},
                                        {verify, verify_none},
                                        {active, false},
                                        {packet, 0}
                                       ], 6000);
                       gen_tcp ->
                           gen_tcp:connect(Host, Port,
                                           [binary, {active, false}, {packet, 0}], 6000)
                   end,
    State = #state{
      host = Host,
      port = Port,
      path = Path,
      transport = Transport,
      handler = Handler,
      socket = Socket
     },
    ok = websocket_handshake(State, HandlerState),
    case Socket of
        {sslsocket, _, _} ->
            ssl:setopts(Socket, [{active, true}]);
        _ ->
            inet:setopts(Socket, [{active, true}])
    end,
    websocket_loop(State, HandlerState, <<>>).

%% @doc Send http upgrade request and validate handshake response challenge
-spec websocket_handshake(State :: tuple(), HandlerState :: any()) ->
    ok.
websocket_handshake(State = #state{path = Path, host = Host}, HandlerState) ->
    Key = generate_ws_key(),
    Handshake = "GET " ++ Path ++ " HTTP/1.1\r\n" ++
        "Host: " ++ Host ++ "\r\n" ++
        "Upgrade: websocket\r\n" ++
        "Connection: Upgrade\r\n" ++
        "Sec-WebSocket-Key: " ++ Key ++ "\r\n" ++
        "Origin: " ++ Host ++ "\r\n" ++
        "Sec-WebSocket-Protocol: \r\n" ++
        "Sec-WebSocket-Version: 13\r\n\r\n",
    Transport = State#state.transport,
    Socket = State#state.socket,
    Transport:send(Socket, Handshake),
    {ok, HandshakeResponse} = Transport:recv(Socket, 0, 6000),
    validate_handshake(HandshakeResponse, Key).

%% @doc Main loop
-spec websocket_loop(State :: tuple(), HandlerState :: any(), Buffer :: binary()) ->
    ok.
websocket_loop(State = #state{remaining = Remaining, socket = Socket},
               HandlerState, Buffer) ->
    receive
        {_TransportType, Socket, Data} ->
            case Remaining of
                undefined ->
                    retrieve_frame(State, HandlerState, << Buffer/binary, Data/binary >>);
                _ ->
                    retrieve_frame(State, HandlerState, State#state.opcode, Remaining, Data, Buffer)
            end;
        Msg ->
            Handler = State#state.handler,
            HandlerResponse = Handler:websocket_info(Msg, HandlerState),
            HandlerState1 = handle_response(State, HandlerResponse),
            websocket_loop(State, HandlerState1, Buffer)
    end,
    ok.

%% @doc Key sent in initial handshake
-spec generate_ws_key() ->
    list().
generate_ws_key() ->
    random:seed(now()),
    base64:encode_to_string(crypto:rand_bytes(16)).

%% @doc Validate handshake response challenge
-spec validate_handshake(HandshakeResponse :: binary(), Key :: list()) ->
    ok.
validate_handshake(HandshakeResponse, Key) ->
    {match, [Challenge]} = re:run(HandshakeResponse, ".*Sec-WebSocket-Accept: (.*)\\r\\n.*",
                                  [{capture, [1], list}]),
    Challenge = base64:encode(crypto:sha(
                                << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>
                               )),
    ok.

%% @doc Mapping from opcode to opcode name and vice versa
-spec websocket_opcode(opcode() | integer()) ->
    integer() | opcode().
websocket_opcode(text) -> 1;
websocket_opcode(1) -> text;
websocket_opcode(binary) -> 2;
websocket_opcode(2) -> binary.

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
%% @doc Length is a 63 bit integer
retrieve_frame(State, HandlerState,
               << 1:1, 0:3, Opcode:4, 0:1, 127:7, Len:63, Rest/bits >>)
  when Len > 16#ffff, Opcode < 8 ->
    retrieve_frame(State, HandlerState, Opcode, Len, Rest, <<>>);
%% @doc Need more data to read length properly
retrieve_frame(State, HandlerState, Data) ->
    websocket_loop(State, HandlerState, Data).

%% @doc Length known and still missing data
retrieve_frame(State, HandlerState, Opcode, Len, Data, Buffer) when byte_size(Data) < Len ->
    Remaining = Len - byte_size(Data),
    websocket_loop(State#state{remaining = Remaining, opcode = Opcode},
                   HandlerState, << Buffer/bits, Data/bits >>);
%% @doc Length known and remaining data is appended to the buffer
retrieve_frame(State, HandlerState, Opcode, Len, Data, Buffer) ->
    << Payload:Len/binary, Rest/bits >> = Data,
    Handler = State#state.handler,
    FullPayload = << Buffer/binary, Payload/binary >>,
    HandlerResponse = Handler:websocket_handle({websocket_opcode(Opcode), FullPayload}, HandlerState),
    HandlerState1 = handle_response(State, HandlerResponse),
    websocket_loop(State#state{remaining = undefined}, HandlerState1, Rest).

%% @doc Handles return values from the callback module
handle_response(_State = #state{socket = Socket, transport = Transport},
                {reply, Frame, HandlerState}) ->
    ok = Transport:send(Socket, encode_frame(Frame)),
    HandlerState;
handle_response(_, {ok, HandlerState}) ->
    HandlerState.

%% @doc Encodes the data with a header (including a masking key) and masks the data
-spec encode_frame(frame()) ->
    binary().
encode_frame({Type, Payload}) ->
    Opcode = websocket_opcode(Type),
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    MaskingKeyBin = crypto:rand_bytes(4),
    << MaskingKey:32 >> = MaskingKeyBin,
    Header = << 1:1, 0:3, Opcode:4, 1:1, BinLen/bits, MaskingKeyBin/bits >>,
    MaskedPayload = mask_payload(MaskingKey, Payload),
    << Header/binary, MaskedPayload/binary >>.

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

-module(wc_SUITE).

-include_lib("common_test/include/ct.hrl").
-define(print(Value), io:format("~n~p~n", [Value])).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).
-export([
         test_text_frames/1,
         test_binary_frames/1,
         test_control_frames/1
        ]).

all() ->
    [
     test_text_frames,
     test_binary_frames,
     test_control_frames
    ].

init_per_suite(Config) ->
    application:start(sasl),
    crypto:start(),
    application:start(ranch),
    application:start(cowboy),
    ok = echo_server:start(),
    io:format("~p", [ws_client:module_info()]),
    Config.

end_per_suite(Config) ->
    Config.

test_text_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Short message
    Short = short_msg(),
    ws_client:send_text(Pid, Short),
    {text, Short} = ws_client:recv(Pid),
    %% Payload length greater than 125 (actual 150).
    Medium = medium_msg(),
    ws_client:send_text(Pid, Medium),
    {text, Medium} = ws_client:recv(Pid),
    %% Payload length greater than 65535
    Long = long_msg(),
    ws_client:send_text(Pid, Long),
    {text, Long} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.

test_binary_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Short message
    Short = short_msg(),
    ws_client:send_binary(Pid, Short),
    {binary, Short} = ws_client:recv(Pid),
    %% Payload length greater than 125 (actual 150).
    Medium = medium_msg(),
    ws_client:send_binary(Pid, Medium),
    {binary, Medium} = ws_client:recv(Pid),
    %% Payload length greater than 65535
    Long = long_msg(),
    ws_client:send_binary(Pid, Long),
    {binary, Long} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.

test_control_frames(_) ->
    {ok, Pid} = ws_client:start_link(),
    %% Send ping with short message
    Short = short_msg(),
    ws_client:send_ping(Pid, Short),
    {pong, Short} = ws_client:recv(Pid),
    %% Server will echo the ping as well
    {ping, Short} = ws_client:recv(Pid),
    {pong, Short} = ws_client:recv(Pid),
    %% Send ping without message
    ws_client:send_ping(Pid, <<>>),
    {pong, <<>>} = ws_client:recv(Pid),
    ws_client:stop(Pid),
    ok.

short_msg() ->
    <<"hello">>.
medium_msg() ->
    <<"ttttttttttttttttttttttttt"
     "ttttttttttttttttttttttttt"
     "ttttttttttttttttttttttttt"
     "ttttttttttttttttttttttttt"
     "ttttttttttttttttttttttttt"
     "ttttttttttttttttttttttttt">>.
long_msg() ->
    Medium = medium_msg(),
    %% 600 bytes
    L = << Medium/binary, Medium/binary, Medium/binary, Medium/binary >>,
    %% 2400 bytes
    L1 = << L/binary, L/binary, L/binary, L/binary >>,
    %% 9600 bytes
    L2 = << L1/binary, L1/binary, L1/binary, L1/binary >>,
    %% 38400 bytes
    L3 = << L2/binary, L2/binary, L2/binary, L2/binary >>,
    %% 76800 bytes
    << L3/binary, L3/binary >>.

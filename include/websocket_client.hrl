-type protocol() :: ws | wss.

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.

-type frame() :: close | ping | pong |
  {text | binary | close | ping | pong, binary()}
  | {close, 1000..4999, binary()}.

-export_type([protocol/0, opcode/0, frame/0]).

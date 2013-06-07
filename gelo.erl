-module(gelo).
-export([string/1]).

string(S) ->
    gelo_translator:start(gelo_parser:tokens(gelo_scanner:scan(S))).

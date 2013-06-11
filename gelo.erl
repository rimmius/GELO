-module(gelo).
-export([string/1, setup/0]).

string(S) ->
    gelo_translator:start(function, gelo_parser:tokens(gelo_scanner:scan(S))).

setup() ->
    yecc:file(gelo_parser),
    leex:file(gelo_scanner),
    c:c(gelo_parser),
    c:c(gelo_scanner),
    c:c(gelo_translator).

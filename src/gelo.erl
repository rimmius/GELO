-module(gelo).
-export([string/1, string/2, setup/0, file/1]).

string(S) ->
    gelo_translator:start(function, gelo_parser:tokens(gelo_scanner:scan(S))).

string(F, S) ->
    gelo_translator:start(F, gelo_parser:tokens(gelo_scanner:scan(S))).

setup() ->
    yecc:file(gelo_parser),
    leex:file(gelo_scanner),
    c:c(gelo_parser),
    c:c(gelo_scanner),
    c:c(gelo_translator).

file(F) ->
    case file:read_file(atom_to_list(F)++".gelo") of
	{ok, B} ->
	    gelo:string(F, binary_to_list(B));
	What ->
	    What
    end.

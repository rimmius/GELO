-module(gelo).
-export([string/1, string/2, setup/0, compile/1, start/0]).

start() ->
    gelo_sup:start_link().

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

compile([F]) ->
    case file:read_file(atom_to_list(F)++".gelo") of
	{ok, B} ->
	    gelo:string(F, binary_to_list(B)),
            io:format("Compilation complete: ~p~n", [F]);
	What ->
	    What
    end.

Definitions.

D  = [0-9]
O  = (\(|\)|\{|\}|\+|-|\*|/|;|=|\[|\]|,|\.)
L = [\_0-9_a-z_A-Z]
St = (\\x{H}+;|\\.|[^"])*
S = [\000-\s\%]

Rules.

fun : {token, {'fun', TokenLine}}.
server.create : {token, {servercreate, TokenLine}}.
server.accept : {token, {serveraccept, TokenLine}}.
server.send : {token, {serversend, TokenLine}}.
server.close : {token, {serverclose, TokenLine}}.
thread : {token, {thread, TokenLine}}.
return : {token, {variable, TokenLine}}.
var : {token, {variable, TokenLine}}.
function : {token, {function, TokenLine}}.
if : {token, {'if', TokenLine}}.
else : {token, {else, TokenLine}}.
console : {token, {console, TokenLine}}.
log : {token, {log, TokenLine}}.
get : {token, {get, TokenLine}}.
and : {token, {'and', TokenLine}}.
or : {token, {'or', TokenLine}}.
spawn : {token, {spawn, TokenLine}}.
thread.send : {token, {threadsend, TokenLine}}.
receive : {token, {recv, TokenLine}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
{L}* : {token, {name, TokenLine, TokenChars}}.
"{St}*" : {token, {string, TokenLine, TokenChars}}.
{O} : {token, {list_to_atom(TokenChars), TokenLine}}.
<   : {token, {lt, TokenLine}}.
>   : {token, {gt, TokenLine}}.
== : {token, {eq, TokenLine}}.
!= : {token, {neq, TokenLine}}.
<= : {token, {leq, TokenLine}}.
>= : {token, {geq, TokenLine}}.
\. : {token, {concat, TokenLine}}.
\@ : {token, {atomic, TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
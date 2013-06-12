Definitions.

D  = [0-9]
O  = (\(|\)|\{|\}|\+|-|\*|/|;|=|,|\.)
L = [a-z_A-Z]
St = [a-z_A-Z\s]
S = [\000-\s]

Rules.

function : {token, {function, TokenLine}}.
if : {token, {'if', TokenLine}}.
else : {token, {else, TokenLine}}.
echo : {token, {echo, TokenLine}}.
list : {token, {list, TokenLine}}.
get : {token, {get, TokenLine}}.
and : {token, {'and', TokenLine}}.
or : {token, {'or', TokenLine}}.
spawn : {token, {spawn, TokenLine}}.
\${L}* : {token, {id, TokenLine, TokenChars}}.
{L}* : {token, {name, TokenLine, TokenChars}}.
"{L}*" : {token, {string, TokenLine, TokenChars}}.
"{St}*" : {token, {string, TokenLine, TokenChars}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
{O} : {token, {list_to_atom(TokenChars), TokenLine}}.
<   : {token, {lt, TokenLine}}.
>   : {token, {gt, TokenLine}}.
== : {token, {eq, TokenLine}}.
!= : {token, {neq, TokenLine}}.
<= : {token, {leq, TokenLine}}.
>= : {token, {geq, TokenLine}}.
\. : {token, {concat, TokenLine}}.
\@ : {token, {ext, TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
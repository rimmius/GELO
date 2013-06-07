Definitions.

D  = [0-9]
O  = (\(|\)|\{|\}|\+|-|\*|/|;|=|,)
L = [a-z_A-Z]
S = [\000-\s]

Rules.

function : {token, {function, TokenLine}}.
if : {token, {'if', TokenLine}}.
else : {token, {else, TokenLine}}.
@{L}* : {token, {id, TokenLine, TokenChars}}.
{L}* : {token, {name, TokenLine, TokenChars}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
{O} : {token, {list_to_atom(TokenChars), TokenLine}}.
<   : {token, {lt, TokenLine}}.
>   : {token, {gt, TokenLine}}.
== : {token, {eq, TokenLine}}.
!= : {token, {neq, TokenLine}}.
<= : {token, {leq, TokenLine}}.
>= : {token, {geq, TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
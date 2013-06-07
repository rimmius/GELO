Definitions.

D  = [0-9]
O  = (\(|\)|\{|\}|\+|-|\*|/|;|=)
L = [a-z_A-Z]
S = [\000-\s]

Rules.

function : {token, {function, TokenLine}}.
{L}* : {token, {id, TokenLine, TokenChars}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
{O} : {token, {list_to_atom(TokenChars), TokenLine}}.
<   : {token, {lt, TokenLine}}.
>   : {token, {gt, TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
Nonterminals Statements Statement Expression math1 math2.
Terminals '+' '-' '*' '/' ';' integer.
Rootsymbol Statements.

Left 100 math1.
Left 200 math2.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'|'$2'].
Statement -> Expression ';' : '$1'.

Expression -> Expression math1 Expression : {'$2', '$1', '$3'}.
Expression -> Expression math2 Expression : {'$2', '$1', '$3'}.
Expression -> integer : {integer, list_to_integer(unwrap('$1'))}.

math1 -> '+' : add.
math1 -> '-' : subtract.
math2 -> '*' : multiply.
math2 -> '/' : divide.

Erlang code.

-export([tokens/1]).

unwrap({_,_,Value}) -> Value.
tokens(Tokens) ->
    {ok, AST} = parse(Tokens),
    AST.

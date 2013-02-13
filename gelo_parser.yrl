Nonterminals Statements Statement Expression math1 math2 assign.
Terminals '+' '-' '*' '/' ';' '=' integer id.
Rootsymbol Statements.

Left 100 math1.
Left 200 math2.
Left 50 assign.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'|'$2'].
Statement -> Expression ';' : '$1'.
Statement -> Expression Expression ';' : ['$1'|'$2'].

Expression -> Expression assign Expression : {'$2', '$1', '$3'}.
Expression -> Expression math1 Expression : {'$2', '$1', '$3'}.
Expression -> Expression math2 Expression : {'$2', '$1', '$3'}.
Expression -> integer : {integer, list_to_integer(unwrap('$1'))}.
Expression -> id : {variable, unwrap('$1')}.

assign -> '=' : assign.
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


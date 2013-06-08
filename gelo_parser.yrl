Nonterminals Functions Function Statements Statement Expression Param Params Comp If Else ElseIf ElseIfs comp math1 math2 assign.
Terminals '+' '-' '*' '/' ';' '=' '(' ')' '{' '}' ',' eq integer id lt gt function 'if' neq leq geq else name.
Rootsymbol Functions.

Left 100 math1.
Left 200 math2.
Left 50 assign.	

Functions -> Function : ['$1'].
Functions -> Function Functions : ['$1'|'$2'].

Function -> function name '(' Params ')' '{' Statements '}' : {function, unwrap('$2'), '$4', '$7'}.
Function -> function name '(' ')' '{' Statements '}' : {function, unwrap('$2'), [], '$6'}.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'|'$2'].
Statement -> Expression ';' : '$1'.
Statement -> Expression Expression ';' : ['$1'|'$2'].
Statement -> If : '$1'.

Params -> Param : ['$1'].
Params -> Param ',' Params : ['$1'|'$3'].
Param -> Expression : '$1'.

If  -> 'if' '(' Comp ')' '{' Statements '}' : {ifs, '$3', '$6'}.
If -> 'if' '(' Comp ')' '{' Statements '}' Else : {ifs, '$3', '$6', '$8'}.
If -> 'if' '(' Comp ')' '{' Statements '}' ElseIfs : {ifs, '$3', '$6', '$8'}.

ElseIfs -> ElseIf : '$1'.
ElseIfs -> ElseIf ElseIf : ['$1'|'$2'].
ElseIf -> else 'if' '(' Comp ')' '{' Statements '}' : {elseif, '$4', '$7'}.

Else -> else '{' Statements '}' : {else, [], '$3'}.

Comp -> Expression comp Expression : {'$2', '$1', '$3'}.

Expression -> Expression assign Expression : {'$2', '$1', '$3'}.
Expression -> Expression math1 Expression : {'$2', '$1', '$3'}.
Expression -> Expression math2 Expression : {'$2', '$1', '$3'}.
Expression -> integer : {integer, list_to_integer(unwrap('$1'))}.
Expression -> id : {variable, unwrap('$1')}.

comp -> lt : lt.
comp -> gt : gt.
comp -> eq : eq.
comp -> neq : neq.
comp -> leq : leq.
comp -> geq : geq.
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


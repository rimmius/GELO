-module(gelo_translator).

-export([start/1]).
-record(info, {exports = []}).

start(S) ->
    Info = #info{},
    {NewInfo, Trans} = do_translate(S, [], Info),
    make_forms(Trans, NewInfo).
do_translate([], Trans, Info) ->
    {Info, Trans};
do_translate([{function, Name, [], Fun}|T], Trans, Info) ->
    Exports = Info#info.exports,
    NewInfo = Info#info{exports = [{list_to_atom(Name), 0}|Exports]},
    do_translate(T, [{function, 1, list_to_atom(Name), 0, [{clause, 1, [], [], do_fun(Fun)}]}|Trans], NewInfo).

do_fun([]) ->
    [];
do_fun([H|T]) ->
    [do_fun(H)|do_fun(T)];
do_fun({variable, Var}) ->
    {var, 1, list_to_atom(Var)};
do_fun({assign, Arg1, Arg2}) ->
    {match, 1, do_fun(Arg1), do_fun(Arg2)};
do_fun({ifs, Arg1, Arg2}) ->
    {'case', 1, do_fun(Arg1), [{clause, 1, [{atom, 1, true}], [], do_fun(Arg2)}]};
do_fun({gt, Arg1, Arg2}) ->
    {op, 1, '>', do_fun(Arg1), do_fun(Arg2)};
do_fun({subtract, Arg1, Arg2}) ->
    {op, 1, '-', do_fun(Arg1), do_fun(Arg2)};
do_fun({add, Arg1, Arg2}) ->
    {op, 1, '+', do_fun(Arg1), do_fun(Arg2)};
do_fun({multiply, Arg1, Arg2}) ->
    {op, 1, '*', do_fun(Arg1), do_fun(Arg2)};
do_fun({divide, Arg1, Arg2}) ->
    {op, 1, '/', do_fun(Arg1), do_fun(Arg2)};
do_fun({integer, N}) ->
    {integer,1,N}.

make_forms(Trans, Info) ->
    {ok, Name, Beam} = compile:forms([{attribute,1,module,a}, {attribute, 1, export, Info#info.exports}|Trans]),
    FileName = atom_to_list(Name) ++ ".beam",
    file:write_file(FileName, Beam),
    c:l(Name).

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
do_fun([{variable, Var}|T]) ->
    [{atom, 1, list_to_atom(Var)}|do_fun(T)];
do_fun([{add, {integer, Nr}, {integer, Nr2}}|T]) ->
    [{op, 1, '+', {integer, 1, Nr}, {integer, 1, Nr2}}|do_fun(T)].

make_forms(Trans, Info) ->
    {ok, Name, Beam} = compile:forms([{attribute,1,module,a}, {attribute, 1, export, Info#info.exports}|Trans]),
    FileName = atom_to_list(Name) ++ ".beam",
    file:write_file(FileName, Beam),
    c:l(Name).

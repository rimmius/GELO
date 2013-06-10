-module(gelo_lib).
-export([get_value/3]).

get_value([H|_T], I, I) ->
    H;
get_value([_H|T], I, A) ->
    get_value(T, I, A+1);
get_value([], _, _) ->
    undefined.

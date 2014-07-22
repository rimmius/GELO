-module(gelo_lib).
-export([ get_value/3
        , server_send/4]).

get_value([H|_T], I, I) ->
    H;
get_value([_H|T], I, A) ->
    get_value(T, I, A+1);
get_value([], _, _) ->
    undefined.

server_send(Socket, Status, Headers, Response0) ->
    Response = list_to_binary(io_lib:fwrite(
                     "HTTP/1.1 ~s ~s ~s, Content-Length: ~p\n\n~s"
                                , [ integer_to_list(Status)
                                  , http_definition(Status)
                                  , Headers
                                  , erlang:length(Response0)
                                  , Response0])
                  ),
    gen_tcp:send(Socket, Response).

http_definition(200) ->
    "OK".

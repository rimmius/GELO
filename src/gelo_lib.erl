-module(gelo_lib).
-export([ get_value/3
        , server_create/2
        , server_send/4]).

get_value([H|_T], I, I) ->
    H;
get_value([_H|T], I, A) ->
    get_value(T, I, A+1);
get_value([], _, _) ->
    undefined.

server_create(Port, Fun) ->
    Listen = gen_tcp:listen(Port, [ {backlog, 1024}
                                  , {send_timeout, 30000}
                                  , {send_timeout_close, true}
                                  , binary
                                  ]),
    server_accept(Listen, Fun).

server_accept({ok, ListenSocket} = Created, Fun) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    server_request(Socket, Fun),
    server_accept(Created, Fun);
server_accept({_, Error}, _) ->
    io:format("Server could not be started with reason: "
              "~p~n", [Error]),
    Error.

server_request(Socket, Fun) ->
    receive
        {tcp, Socket, Request0} ->
            {ok, Packet, _R} = erlang:decode_packet(http, Request0, []),
            Request = decode(Packet),
            Fun(Request, Socket),
            gen_tcp:close(Socket);
        _ ->
            ok
    end.

decode({http_request, Method, {abs_path, Url}, _Version}) ->
    [{method, atom_to_list(Method)}, {url, Url}].

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
    "OK";
http_definition(201) ->
    "Created";
http_definition(202) ->
    "Accepted";
http_definition(203) ->
    "Non-Authoritative Information";
http_definition(204) ->
    "No Content";
http_definition(205) ->
    "Reset Content";
http_definition(206) ->
    "Partial Content";
http_definition(300) ->
    "Multiple Choices";
http_definition(301) ->
    "Moved Permanently";
http_definition(302) ->
    "Found";
http_definition(303) ->
    "See Other";
http_definition(304) ->
    "Not Modified";
http_definition(305) ->
    "Use Proxy";
http_definition(307) ->
    "Temporary Redirect";
http_definition(400) ->
    "Bad Request";
http_definition(401) ->
    "Unauthorized";
http_definition(402) ->
    "Payment Required";
http_definition(403) ->
    "Forbidden";
http_definition(404) ->
    "Not Found";
http_definition(405) ->
    "Method Not Allowed";
http_definition(406) ->
    "Not Acceptable";
http_definition(407) ->
    "Proxy Authentication Required";
http_definition(408) ->
    "Request Timeout";
http_definition(409) ->
    "Conflict";
http_definition(410) ->
    "Gone";
http_definition(411) ->
    "Length Required";
http_definition(412) ->
    "Precondition Failed";
http_definition(413) ->
    "Request Entity Too Large";
http_definition(414) ->
    "Request-URI Too Long";
http_definition(415) ->
    "Unsupported Media Type";
http_definition(416) ->
    "Requested Range Not Satisfiable";
http_definition(417) ->
    "Expectation Failed";
http_definition(500) ->
    "Internal Server Error";
http_definition(501) ->
    "Not Implemented";
http_definition(502) ->
    "Bad Gateway";
http_definition(503) ->
    "Service Unavailable";
http_definition(504) ->
    "Gateway Timeout";
http_definition(505) ->
    "HTTP Version Not Supported".

%%%----------------------------------------------------------------------
%% @copyright Hunter Morris
%% @author Hunter Morris <huntermorris@gmail.com>
%% @version {@vsn}, {@date}, {@time}
%% @doc ewgi inets server gateway
%%
%% See LICENSE file in this source package
%%%----------------------------------------------------------------------

-module(ewgi_inets).

%% ewgi callbacks
-export([req/2, rsp/3]).

-ifdef(HAS_R14).
-include_lib("inets/src/http_server/httpd.hrl").
-else.
-include_lib("inets/src/httpd.hrl").
-endif.
-include("ewgi.hrl").

req(#mod{}=Arg, Opts) ->
    R = ewgi:new_req(),
    Folder = fun(F, Req) -> F(Arg, Opts, Req) end,
    lists:foldl(Folder, R, [fun method/3,
                            fun script_name/3,
                            fun path/3,
                            fun url_scheme/3,
                            fun server_pair/3,
                            fun peer_pair/3,
                            fun headers/3,
                            fun input/3,
                            fun errors/3]).

rsp(#mod{}=Arg, _Opts, #ewgi_rsp{}=Rsp) ->
    {Code, _Msg} = ewgi:status(Rsp),
    HeaderList = ewgi:rsp_headers(Rsp),
    case ewgi:body(Rsp) of
        {file, _Path} ->
            % TODO: Check path
            mod_get:do(Arg);
        F when is_function(F, 0) ->
            rsp_stream(F, Arg, Rsp);
        Iol ->
            Size = integer_to_list(httpd_util:flatlength(Iol)),
            Headers0 = [{code, Code}, {content_length, Size}],
            Headers = lists:foldl(fun rsp_headers/2, Headers0,
                                  HeaderList),
            {break, [{response, {response, Headers, Iol}}]}
    end.

rsp_headers({"accept-ranges", V}, L) ->
    [{accept_ranges, V}|L];
rsp_headers({"allow", V}, L) ->
    [{allow, V}|L];
rsp_headers({"cache-control", V}, L) ->
    [{cache_control, V}|L];
rsp_headers({"content-MD5", V}, L) ->
    [{content_MD5, V}|L];
rsp_headers({"content-encoding", V}, L) ->
    [{content_encoding, V}|L];
rsp_headers({"content-language", V}, L) ->
    [{content_language, V}|L];
rsp_headers({"content-length", _V}, L) ->
    % [{content_length, V}|L];
    L;
rsp_headers({"content-location", V}, L) ->
    [{content_location, V}|L];
rsp_headers({"content-range", V}, L) ->
    [{content_range, V}|L];
rsp_headers({"content-type", V}, L) ->
    [{content_type, V}|L];
rsp_headers({"date", V}, L) ->
    [{date, V}|L];
rsp_headers({"etag", V}, L) ->
    [{etag, V}|L];
rsp_headers({"expires", V}, L) ->
    [{expires, V}|L];
rsp_headers({"last-modified", V}, L) ->
    [{last_modified, V}|L];
rsp_headers({"location", V}, L) ->
    [{location, V}|L];
rsp_headers({"pragma", V}, L) ->
    [{pragma, V}|L];
rsp_headers({"retry-after", V}, L) ->
    [{retry_after, V}|L];
rsp_headers({"server", V}, L) ->
    [{server, V}|L];
rsp_headers({"trailer", V}, L) ->
    [{trailer, V}|L];
rsp_headers({"transfer-encoding", V}, L) ->
    [{transfer_encoding, V}|L];
rsp_headers({Header, Val}, L) when is_list(Header), is_list(Val) ->
    % TODO: Check unknown/unused header values?
    [{Header, Val}|L].

rsp_stream(_F, _Arg, _Rsp) ->
    ok.

method(#mod{method=M}, _Opts, R0) ->
    Method = ewgi_util:list_to_method(M),
    ewgi:method(Method, R0).

script_name(_Arg, Opts, R0) ->
    Script = proplists:get_value(script, Opts, []),
    ewgi:script_name(Script, R0).

path(#mod{request_uri=U}, _Opts, R) ->
    path_fold(U, R).

path_fold('*', R0) ->
    R = ewgi:path_info("*", R0),
    ewgi:query_string([], R);
path_fold(Raw, R0) ->
    {Path, Query, _Frag} = mochiweb_util:urlsplit_path(Raw),
    R = ewgi:path_info(Path, R0),
    ewgi:query_string(Query, R).

url_scheme(#mod{socket_type={ssl, _}}, _Opts, R) ->
    ewgi:url_scheme("https", R);
url_scheme(#mod{}, _Opts, R) ->
    ewgi:url_scheme("http", R).

server_pair(#mod{socket_type={ssl, _}, socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:ssl_server_pair(Socket),
    R = ewgi:server_port(Port, R0),
    ewgi:server_name(Name, R);
server_pair(#mod{socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_server_pair(Socket),
    R = ewgi:server_port(Port, R0),
    ewgi:server_name(Name, R).

peer_pair(#mod{socket_type={ssl, _}, socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:ssl_peer_pair(Socket),
    R = ewgi:peer_port(Port, R0),
    ewgi:peer_name(Name, R);
peer_pair(#mod{socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_peer_pair(Socket),
    R = ewgi:peer_port(Port, R0),
    ewgi:peer_name(Name, R).

headers(#mod{parsed_header=H}, _Opts, R0) ->
    lists:foldl(fun({K, V}, Req) ->
                        ewgi:add_req_header(K, V, Req)
                end, R0, H).

input(#mod{entity_body=Body}, _Opts, R0) ->
    F = ewgi_util:read_bin_input(Body),
    ewgi:input(F, R0).

errors(#mod{}, _Opts, R0) ->
    ewgi:errors(fun error_logger:error_report/1, R0).

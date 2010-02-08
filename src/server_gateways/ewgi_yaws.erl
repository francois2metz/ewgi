%%%----------------------------------------------------------------------
%% @copyright Hunter Morris
%% @author Hunter Morris <huntermorris@gmail.com>
%% @version {@vsn}, {@date}, {@time}
%% @doc ewgi Yaws server gateway
%%
%% See LICENSE file in this source package
%%%----------------------------------------------------------------------

-module(ewgi_yaws).

-export([req/2, rsp/3]).

-include("ewgi.hrl").
-include("yaws_api.hrl").

-spec req(Arg::term(), Opts::list()) -> #ewgi_req{}.

req(#arg{}=Arg, Opts) ->
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

-spec rsp(Arg::term(), Opts::list(), Rsp::#ewgi_rsp{}) -> any().

rsp(_Arg, _Opts, Rsp) when is_record(Rsp, ewgi_rsp) ->
    {Code, _Msg} = ewgi:status(Rsp),
    Headers = ewgi:rsp_headers(Rsp),
    {Ct, Hdrs} = yaws_rsp_headers(Headers),
    case ewgi:body(Rsp) of
        {file, Path} when is_list(Path) ->
            []; % TODO: File serving
        F when is_function(F, 0) ->
            []; % TODO: Chunked response
        Iol when is_list(Iol); is_binary(Iol) ->
            [{status, Code},
             {allheaders, Hdrs},
             {content, Ct, Iol}]
    end.

yaws_rsp_headers(H) when is_list(H) ->
    lists:foldl(fun({K0, V}, Acc) ->
                        K = string:to_lower(K0),
                        yaws_rsp_header(K, V, Acc)
                end, {"text/plain", []}, H).

yaws_rsp_header("content-type", V, {_Ct, Acc}) ->
    {V, Acc};
yaws_rsp_header(K, V, {Ct, Acc}) ->
    {Ct, [{header, [K, ": ", V]}|Acc]}.

method(#arg{req=#http_request{method=M}}, _Opts, _R) ->
    ewgi:method(M).

script_name(#arg{prepath=V}, _Opts, _R) ->
    ewgi:script_name(V).

path(#arg{pathinfo=undefined, querydata=Q}, _Opts, R0) ->
    R = ewgi:path_info("/", R0),
    ewgi:query_string(Q, R);
path(#arg{pathinfo=P, querydata=Q}, _Opts, R0) ->
    R = ewgi:path_info(P, R0),
    ewgi:query_string(Q, R).

url_scheme(#arg{}, _Opts, R) ->
    % Possible to do SSL in Yaws?
    ewgi:url_scheme("http", R).

server_pair(#arg{clisock=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_server_pair(Socket),
    R = ewgi:server_port(Port, R0),
    ewgi:server_name(Name, R).

peer_pair(#arg{clisock=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_peer_pair(Socket),
    R = ewgi:server_port(Port, R0),
    ewgi:server_name(Name, R).

headers(#arg{headers=H}, _Opts, R0) ->
    Headers = [{"connection", H#headers.connection},
               {"accept", H#headers.accept},
               {"host", H#headers.host},
               {"if-modified-since", H#headers.if_modified_since},
               {"if-match", H#headers.if_match},
               {"if-none-match", H#headers.if_none_match},
               {"if-range", H#headers.if_range},
               {"if-unmodified-since", H#headers.if_unmodified_since},
               {"range", H#headers.range},
               {"referer", H#headers.referer},
               {"user-agent", H#headers.user_agent},
               {"accept-ranges", H#headers.accept_ranges},
               {"keep-alive", H#headers.keep_alive},
               {"location", H#headers.location},
               {"content-length", H#headers.content_length},
               {"content-type", H#headers.content_type},
               {"content-encoding", H#headers.content_encoding},
               {"authorization", H#headers.authorization},
               {"transfer-encoding", H#headers.transfer_encoding},
               {"cookie", H#headers.cookie}],
    Other = [{K, V} || {http_header, _, K, V} <- H#headers.other],
    lists:foldl(fun({K, V}, Req) ->
                        ewgi:add_req_header(K, V, Req)
                end, R0, Headers ++ Other).

input(#arg{clidata=D}, _Opts, R) ->
    F = ewgi_util:read_bin_input(D),
    ewgi:read_input(F, R).

errors(#arg{}, _Opts, R) ->
    ewgi:errors(fun error_logger:error_report/1, R).

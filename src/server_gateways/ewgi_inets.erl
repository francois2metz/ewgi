%%%-------------------------------------------------------------------
%%% File    : ewgi_inets.erl
%%% Author  : Hunter Morris <huntermorris@gmail.com>
%%% License :
%%% The contents of this file are subject to the Mozilla Public
%%% License Version 1.1 (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of
%%% the License at http://www.mozilla.org/MPL/
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and
%%% limitations under the License.
%%% The Initial Developer of the Original Code is S.G. Consulting
%%% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
%%% 2007 S.G. Consulting srl. All Rights Reserved.
%%%
%%% @doc 
%%% <p>Reference implementation of a inets EWGI server gateway.</p>
%%% <p>Requires that environment variables 'app_module' and
%%% 'app_function' are set for application 'ewgi'</p>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ewgi_inets).

%% ewgi callbacks
-export([req/2, rsp/3]).

-include("inets/src/httpd.hrl").
-include("ewgi.hrl").

req(Arg, Opts) when is_record(Arg, mod) ->
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

rsp(Arg, _Opts, Rsp) when is_record(Rsp, ewgi_rsp) ->
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
    ewgi:server_name(Name, R).
server_pair(#mod{socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_server_pair(Socket),
    R = ewgi:server_port(Port, R0),
    ewgi:server_name(Name, R).

peer_pair(#mod{socket_type={ssl, _}, socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:ssl_peer_pair(Socket),
    R = ewgi:peer_port(Port, R0),
    ewgi:peer_name(Name, R).
peer_pair(#mod{socket=Socket}, _Opts, R0) ->
    {Name, Port} = ewgi_util:socket_peer_pair(Socket),
    R = ewgi:peer_port(Port, R0),
    ewgi:peer_name(Name, R).

headers(#mod{parsed_header=H}, _Opts, R0) ->
    lists:foldl(fun({K, V}, Req) ->
                        ewgi:add_req_header(K, V, Req)
                end, R0, H).

input(#mod{entity_body=Body}, _Opts, R0) ->
    F = fun(Callback, Length) ->
                input1(Body, Callback, Length)
        end,
    ewgi:input(F, R0).

input1(_Body, Callback, {Length, _ChunkSz})
  when is_function(Callback, 1), Length =< 0 ->
    Callback(eof);

input1(Body, Callback, {Length, ChunkSz})
  when is_function(Callback, 1), is_binary(Body) ->
    L = if Length > 0, Length < ChunkSz -> Length;
           true -> ChunkSz
        end,
    <<Bin:L/bytes, Rest/bits>> = Body,
    Rem = Length - size(Bin),
    Callback1 = Callback({data, Bin}),
    input1(Rest, Callback1, {Rem, ChunkSz}).

errors(#mod{}, _Opts, R0) ->
    ewgi:errors(fun error_logger:error_report/1, R0).

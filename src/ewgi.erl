%%%----------------------------------------------------------------------
%% @copyright Hunter Morris
%% @author Hunter Morris <huntermorris@gmail.com>
%% @version {@vsn}, {@date}, {@time}
%% @doc ewgi API
%%
%% See LICENSE file in this source package
%%%----------------------------------------------------------------------
-module(ewgi).

-include_lib("ewgi.hrl").

-export([new/0]).

-export([new_req/0]).
-export([req/1, req/2]).
-export([version/1]).
-export([method/1, method/2]).
-export([url_scheme/1, url_scheme/2]).
-export([script_name/1, script_name/2]).
-export([path_info/1, path_info/2]).
-export([query_string/1, query_string/2]).
-export([server_name/1, server_name/2]).
-export([server_port/1, server_port/2]).
-export([peer_name/1, peer_name/2]).
-export([peer_port/1, peer_port/2]).
-export([req_headers/1, req_header/2]).
-export([add_req_header/3, get_req_header/2,
         get_req_header_merged/2]).
-export([input/1, input/2]).
-export([errors/1, errors/2]).
-export([data/1, data/2]).

-export([new_rsp/0]).
-export([rsp/1, rsp/2]).
-export([status/1, status/2]).
-export([rsp_headers/1]).
-export([body/1, body/2]).

%% ----------------------------------------------------------------------
%% General context methods

-spec new() -> #ewgi{}.
    
new() ->
    #ewgi{req=new_req(), rsp=new_rsp()}.

-spec req(#ewgi{}) -> #ewgi_req{}.

req(#ewgi{req=R}) ->
    R.

-spec req(#ewgi_req{}, #ewgi{}) -> #ewgi{}.

req(R, E) when is_record(R, ewgi_req), is_record(E, ewgi) ->
    E#ewgi{req=R}.

-spec rsp(#ewgi{}) -> #ewgi_rsp{}.

rsp(#ewgi{rsp=R}) ->
    R.

-spec rsp(#ewgi_rsp{}, #ewgi{}) -> #ewgi{}.

rsp(R, E) when is_record(R, ewgi_rsp), is_record(E, ewgi) ->
    E#ewgi{rsp=R}.

%% ----------------------------------------------------------------------
%% Request methods

-spec new_req() -> #ewgi_req{}.

new_req() ->
    #ewgi_req{version=?EWGI_VERSION,
              headers=gb_trees:empty(),
              data=gb_trees:empty()}.

-spec version(#ewgi_req{}) -> ewgi_version().

version(#ewgi_req{version=V}) ->
    V.

-spec method(#ewgi_req{}) -> ewgi_request_method().

method(#ewgi_req{method=M}) ->
    M.

-spec method(ewgi_request_method(), #ewgi_req{}) -> #ewgi_req{}.

method(M, #ewgi_req{}=R)
  when M =:= 'OPTIONS'
       ; M =:= 'GET'
       ; M =:= 'HEAD'
       ; M =:= 'POST'
       ; M =:= 'PUT'
       ; M =:= 'DELETE'
       ; M =:= 'TRACE'
       ; M =:= 'CONNECT'
       ; is_list(M) ->
    R#ewgi_req{method=M}.

-spec url_scheme(#ewgi_req{}) -> string().

url_scheme(#ewgi_req{url_scheme=V}) ->
    V.

-spec url_scheme(string(), #ewgi_req{}) -> #ewgi_req{}.

url_scheme(Scheme, #ewgi_req{}=R) when is_list(Scheme) ->
    R#ewgi_req{url_scheme=Scheme}.

-spec script_name(#ewgi_req{}) -> string().

script_name(#ewgi_req{script_name=V}) ->
    V.

-spec script_name(string(), #ewgi_req{}) -> #ewgi_req{}.

script_name(Name, #ewgi_req{}=R) when is_list(Name) ->
    R#ewgi_req{script_name=Name}.

-spec path_info(#ewgi_req{}) -> string().

path_info(#ewgi_req{path_info=V}) ->
    V.

-spec path_info(string(), #ewgi_req{}) -> #ewgi_req{}.

path_info(P, #ewgi_req{}=R) when is_list(P) ->
    R#ewgi_req{path_info=P}.

-spec query_string(#ewgi_req{}) -> string().

query_string(#ewgi_req{query_string=V}) ->
    V.

-spec query_string(string(), #ewgi_req{}) -> #ewgi_req{}.

query_string(Qs, #ewgi_req{}=R) ->
    R#ewgi_req{query_string=Qs}.

-spec server_name(#ewgi_req{}) -> string().

server_name(#ewgi_req{server_name=V}) ->
    V.

-spec server_name(string(), #ewgi_req{}) -> #ewgi_req{}.

server_name(Name, #ewgi_req{}=R) when is_list(Name) ->
    R#ewgi_req{server_name=Name}.

-spec server_port(#ewgi_req{}) -> integer().

server_port(#ewgi_req{server_port=V}) ->
    V.

-spec server_port(integer(), #ewgi_req{}) -> #ewgi_req{}.

server_port(Port, #ewgi_req{}=R) when is_integer(Port) ->
    R#ewgi_req{server_port=Port}.

-spec peer_name(#ewgi_req{}) -> string().

peer_name(#ewgi_req{peer_name=V}) ->
    V.

-spec peer_name(string(), #ewgi_req{}) -> #ewgi_req{}.

peer_name(Name, #ewgi_req{}=R) when is_list(Name) ->
    R#ewgi_req{peer_name=Name}.

-spec peer_port(#ewgi_req{}) -> integer().

peer_port(#ewgi_req{peer_port=V}) ->
    V.

-spec peer_port(integer(), #ewgi_req{}) -> #ewgi_req{}.

peer_port(Port, #ewgi_req{}=R) when is_integer(Port) ->
    R#ewgi_req{peer_port=Port}.

-spec req_headers(#ewgi_req{}) -> bag().

req_headers(#ewgi_req{headers=V}) ->
    V.

-spec req_header(string(), #ewgi_req{}) -> string() | 'undefined'.

req_header(Header, #ewgi_req{headers=H}) ->
    case gb_trees:lookup(Header, H) of
        none -> undefined;
        {value, V} -> V
    end.

-spec add_req_header(string(), string(), #ewgi_req{}) -> #ewgi_req{}.

add_req_header(K, V, #ewgi_req{headers=T0}=R)
  when is_list(K), is_list(V) ->
    R#ewgi_req{headers=add_header(K, V, T0)}.

add_header(Name0, V, T) ->
    Name = string:to_lower(Name0),
    case gb_trees:lookup(Name, T) of
        none ->
            gb_trees:insert(Name, [{Name0, V}], T);
        {value, L} ->
            gb_trees:update(Name, [{Name0, V}|L], T)
    end.

-spec get_req_header(string(), #ewgi_req{}) -> [{string(), string()}].

get_req_header(K, #ewgi_req{headers=T}) when is_list(K) ->
    get_header(K, T).

get_header(Name0, T) ->
    Name = string:to_lower(Name0),
    case gb_trees:lookup(Name, T) of
        none -> [];
        {value, L} when is_list(L) -> L
    end.

-spec get_req_header_merged(string(), #ewgi_req{}) -> string().

get_req_header_merged(K, #ewgi_req{headers=T}) ->
    get_header_merged(K, T).

get_header_merged(Name, T) when is_list(Name) ->
    merge(get_header(Name, T), []).

merge([], Acc) ->
    Acc;
merge([{_K, V}|T], Acc) ->
    merge(T, Acc ++ ", " ++ V).

-spec input(#ewgi_req{}) -> ewgi_read_input().

input(#ewgi_req{input=V}) ->
    V.

-spec input(ewgi_read_input(), #ewgi_req{}) -> #ewgi_req{}.

input(F, #ewgi_req{}=R) when is_function(F, 2) ->
    R#ewgi_req{input=F}.

-spec errors(#ewgi_req{}) -> ewgi_write_error().

errors(#ewgi_req{errors=V}) ->
    V.

-spec errors(ewgi_write_error(), #ewgi_req{}) -> #ewgi_req{}.

errors(F, #ewgi_req{}=R) when is_function(F, 1) ->
    R#ewgi_req{errors=F}.

-spec data(#ewgi_req{}) -> bag().

data(#ewgi_req{data=V}) ->
    V.

-spec data(bag(), #ewgi_req{}) -> #ewgi_req{}.

data(T, #ewgi_req{}=R) ->
    R#ewgi_req{data=T}.

%% ----------------------------------------------------------------------
%% Response methods

-spec new_rsp() -> #ewgi_rsp{}.

new_rsp() ->
    #ewgi_rsp{headers=[], body={}}.

-spec status(#ewgi_rsp{}) -> ewgi_status().

status(#ewgi_rsp{status=S}) ->
    S.

-spec status(ewgi_status(), #ewgi_rsp{}) -> #ewgi_rsp{}.

status({Code, Msg}=Status, #ewgi_rsp{}=R)
  when is_integer(Code), is_list(Msg) ->
    R#ewgi_rsp{status=Status}.

-spec rsp_headers(#ewgi_rsp{}) -> ewgi_header_list().

rsp_headers(#ewgi_rsp{headers=H}) ->
    H.

-spec body(#ewgi_rsp{}) -> ewgi_message_body().

body(#ewgi_rsp{body=B}) ->
    B.

-spec body(ewgi_message_body(), #ewgi_rsp{}) -> #ewgi_rsp{}.

body(Body, #ewgi_rsp{}=R)
  when is_list(Body); is_binary(Body); is_function(Body, 0) ->
    R#ewgi_rsp{body=Body}.
    

%%%----------------------------------------------------------------------
%% @copyright Hunter Morris
%% @author Hunter Morris <huntermorris@gmail.com>
%% @version {@vsn}, {@date}, {@time}
%% @doc ewgi utility functions
%%
%% See LICENSE file in this source package
%%%----------------------------------------------------------------------

-module(ewgi_util).

-export([list_to_method/1]).
-export([socket_server_pair/1]).

-spec list_to_method(Method::string()) -> ewgi_request_method().

list_to_method("OPTIONS") ->
    'OPTIONS';
list_to_method("GET") ->
    'GET';
list_to_method("HEAD") ->
    'HEAD';
list_to_method("POST") ->
    'POST';
list_to_method("PUT") ->
    'PUT';
list_to_method("DELETE") ->
    'DELETE';
list_to_method("TRACE") ->
    'TRACE';
list_to_method("CONNECT") ->
    'CONNECT';
list_to_method(Other) when is_list(Other) ->
    Other.

-spec socket_server_pair(Socket::term()) -> {string(), integer()}.

socket_server_pair(Socket) ->
    {ok, {Addr, Port}} = inet:sockname(Socket),
    Name = inet_parse:ntoa(Addr),
    {Name, Port}.

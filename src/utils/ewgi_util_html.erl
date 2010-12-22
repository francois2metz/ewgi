%%%----------------------------------------------------------------------
%% @copyright Hunter Morris
%% @author Hunter Morris <huntermorris@gmail.com>
%% @version {@vsn}, {@date}, {@time}
%% @doc HTML utility methods
%%
%% See LICENSE file in this source package
%%
%% Some code (including parse_cookie/1) taken from MochiWeb:
%% Emad El-Haraty <emad@mochimedia.com>
%% Copyright 2007 Mochi Media, Inc.
%%%----------------------------------------------------------------------

-module(ewgi_util_html).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([escape/1]).

-define(QUOTE, 34).

%% @spec escape(binary() | string()) -> binary() | string()
%% @doc Replace the special characters '&lt;', '&gt;', '&amp;', and '&quot;'
%% to HTML entity sequences.
-spec(escape/1 :: (binary() | string()) -> binary() | string()).

escape(Bin) when is_binary(Bin) ->
    list_to_binary(escape(binary_to_list(Bin), []));
escape(S) when is_list(S) ->
    escape(S, []).

-spec(escape/2 :: (string(), list()) -> string()).

escape([], Acc) ->
    lists:reverse(Acc);
escape([$<|Rest], Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape([$>|Rest], Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape([$&|Rest], Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([?QUOTE|Rest], Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape([C|Rest], Acc) ->
    escape(Rest, [C|Acc]).

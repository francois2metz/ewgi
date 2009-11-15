-ifndef(_EWGI_HRL).
-define(_EWGI_HRL, 1).

% ``The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
% License for the specific language governing rights and limitations
% under the License.
% 
% The Original Code is the EWGI reference implementation.
% 
% The Initial Developer of the Original Code is S.G. Consulting
% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
% 2007 S.G. Consulting srl. All Rights Reserved.
% 
% Contributor(s): Filippo Pacini <filippo.pacini@gmail.com>
%                 Hunter Morris <huntermorris@gmail.com>

-define(DEFAULT_CHUNKSIZE, 4096).
-define(EWGI_VERSION, {3, 0}).

-type ewgi_propval() :: atom() | integer() | string() | binary().
-type ewgi_prop() :: {ewgi_propval(), ewgi_propval()}.
-type ewgi_proplist() :: [ewgi_prop()].

%% @type bag() = gb_tree()
-ifdef(NO_GB_TREE_SPEC).
-type bag() :: {non_neg_integer(), {any(), any(), any(), any()} | 'nil'}.
-else.
-type bag() :: gb_tree().
-endif.

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | ewgi_ri_callback()).
%% @type ewgi_ri_callback() = function()
-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | function()) | iolist().

%% @type ewgi_read_input() = function()
-type ewgi_read_input() :: fun((ewgi_ri_callback(), integer()) -> ewgi_ri_callback()).

%% @type ewgi_write_error() = function()
-type ewgi_write_error() :: fun((any()) -> 'ok').

%% @type ewgi_version() = {integer(), integer()}
-type ewgi_version() :: {integer(), integer()}.

%% @type ewgi_spec() = {'ewgi_spec', function(), function(), string(),
%%                      ewgi_version(), bag()}

%% @type ewgi_header_val() = string() | 'undefined'
-type ewgi_header_val() :: string() | 'undefined'.

%% @type ewgi_header_key() = string()
-type ewgi_header_key() :: string().

%% @type ewgi_request_method() = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%       'DELETE' | 'TRACE' | 'CONNECT' | string()
-type ewgi_request_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
                               'DELETE' | 'TRACE' | 'CONNECT' | string().

%% @type ewgi_val() = string() | 'undefined'
-type ewgi_val() :: string() | 'undefined'.

%% @type ewgi_request() :: {'ewgi_request', ewgi_val(), integer(), ewgi_val(),
%%                          ewgi_spec(), ewgi_val(), ewgi_http_headers(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_request_method(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val()}

-record(ewgi_req, {
          version       :: ewgi_version(),
          method        :: ewgi_request_method(),
          url_scheme    :: string(),
          script_name   :: string(),
          path_info     :: string(),
          query_string  :: string(),
          server_name   :: string(),
          server_port   :: integer(),
          headers       :: bag(),
          input         :: ewgi_read_input(),
          errors        :: ewgi_write_error(),
          data          :: bag()
         }).

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type stream() :: fun(() -> {} | {any(), stream()}).
%% @type stream() = function()
-type stream() :: fun(() -> {} | {any(), function()}).

%% @type ewgi_status() = {integer(), string()}
-type ewgi_status() :: {integer(), string()}.

%% @type ewgi_message_body() = binary() | iolist() | stream()
-type ewgi_message_body() :: binary() | iolist() | stream().

%% @type ewgi_header_list() = [{ewgi_header_key(), ewgi_header_val()}]
-type ewgi_header_list() :: [{ewgi_header_key(), ewgi_header_val()}].

-record(ewgi_rsp, {
          status  :: ewgi_status(),
          headers :: ewgi_header_list(),
          body    :: ewgi_message_body()
         }).

-record(ewgi, {
          req,
          rsp
         }).

%% @type ewgi_app() = function()
-type ewgi_app() :: fun((ewgi_context()) -> ewgi_context()).

-ifndef(debug).
-define(INSPECT_EWGI_RESPONSE(Ctx), Ctx).
-else.
-define(INSPECT_EWGI_RESPONSE(Ctx),
	begin
	    error_logger:info_msg("Inpecting the final ewgi_response()...~n"
				  "Requested Url: ~p~n"
				  "Status: ~p~n"
				  "Headers: ~p~n"
				  "Body: ~p~n",
				  [ewgi_api:path_info(Ctx),
				   ewgi_api:response_status(Ctx),
				   ewgi_api:response_headers(Ctx),
				   ewgi_api:response_message_body(Ctx)]),
	    Ctx
	end
       ).
-endif.

-endif.

%% See LICENSE in this source package

-ifndef(_EWGI_HRL).
-define(_EWGI_HRL, 1).

-define(DEFAULT_CHUNKSIZE, 4096).
-define(EWGI_VERSION, {3, 0}).

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
-type ewgi_version() :: {non_neg_integer(), non_neg_integer()}.

%% @type ewgi_header_val() = string()
-type ewgi_header_val() :: string().

%% @type ewgi_header_key() = string()
-type ewgi_header_key() :: string().

%% @type ewgi_request_method() = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%       'DELETE' | 'TRACE' | 'CONNECT' | string()
-type ewgi_request_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
                               'DELETE' | 'TRACE' | 'CONNECT' | string().

% Request record
-record(ewgi_req, {
          version       :: ewgi_version(),
          method        :: ewgi_request_method(),
          url_scheme    :: string(),
          script_name   :: string(),
          path_info     :: string(),
          query_string  :: string(),
          server_name   :: string(),
          server_port   :: integer(),
          peer_name     :: string(),
          peer_port     :: integer(),
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

%% @type ewgi_message_body() = binary() | iolist() | stream() | {'file', string()}
-type ewgi_message_body() :: binary() | iolist() | stream() | {'file', string()}.

%% @type ewgi_header_list() = [{ewgi_header_key(), ewgi_header_val()}]
-type ewgi_header_list() :: [{ewgi_header_key(), ewgi_header_val()}].

% Response record
-record(ewgi_rsp, {
          status  :: ewgi_status(),
          headers :: ewgi_header_list(),
          body    :: ewgi_message_body()
         }).

% Context record (request/response pair)
-record(ewgi, {
          req     :: #ewgi_req{},
          rsp     :: #ewgi_rsp{}
         }).

%% @type ewgi_app() = function()
-type ewgi_app() :: fun((#ewgi{}) -> #ewgi{}).

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
				  [ewgi:path_info(Ctx),
				   ewgi:response_status(Ctx),
				   ewgi:rsp_headers(Ctx),
				   ewgi:body(Ctx)]),
	    Ctx
	end
       ).
-endif.

-endif.

-module(cb_onbill_proforma).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(CB_PROFORMA_INVOICE, <<"onbills/proforma_invoice">>).
-define(ATTACHMENT, <<"attachment">>).
-define(GENERATE, <<"generate">>).
-define(MODB, <<"onbills_modb">>).
-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                               %   ,{<<"text">>, <<"plain">>}
                                 ]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_proforma">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_proforma">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_proforma">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_proforma">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET,?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_,_) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(),path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,_) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_) ->
    Context.
content_types_provided(Context,_,?ATTACHMENT) ->
    CTP = [{'to_binary', [{<<"application">>, <<"pdf">>}]}],
    cb_context:set_content_types_provided(Context, CTP).

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbill_poforma(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_onbill_poforma(Context, Id, cb_context:req_verb(Context)).
validate(Context, Id, ?ATTACHMENT) ->
    validate_onbill_poforma(Context, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_onbill_poforma(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill_poforma(Context, ?HTTP_GET) ->
    summary(Context);
validate_onbill_poforma(Context, ?HTTP_PUT) ->
    Context.

validate_onbill_poforma(Context, <<Year:4/binary, Month:2/binary, _/binary>> = Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month))).

validate_onbill_poforma(Context, Id, ?ATTACHMENT, ?HTTP_GET) ->
    load_attachment(Context, Id).

-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    {Year, Month, _} = erlang:date(),
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    onbill_util:maybe_add_design_doc(Modb, <<"onbills">>),
    crossbar_doc:load_view(?CB_PROFORMA_INVOICE, []
                          ,cb_context:set_account_db(Context, Modb)
                          ,fun onbill_util:normalize_view_results/2).

load_attachment(Context0, <<Year:4/binary, Month:2/binary, _/binary>> = Id) ->
    Context = crossbar_doc:load(Id, cb_context:set_account_modb(Context0, kz_term:to_integer(Year), kz_term:to_integer(Month))),
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    case onbill_util:get_attachment(Id, Modb) of
        {'ok', Attachment} ->
            cb_context:set_resp_etag(
                cb_context:set_resp_headers(cb_context:setters(Context
                                                              ,[{fun cb_context:set_resp_data/2, Attachment}
                                                               ,{fun cb_context:set_resp_etag/2, 'undefined'}
                                                               ])
                                            ,[{<<"Content-Disposition">>, <<"attachment; filename="
                                                                            ,(kz_term:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(kz_term:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(kz_term:to_binary(Id))/binary>>
                                              }
                                             ,{<<"Content-Type">>, <<"application/pdf">>}
                                             |cb_context:resp_headers(Context)
                                           ])
                ,'undefined'
            );
        _ ->
            cb_context:add_system_error('faulty_request', Context0)
    end.

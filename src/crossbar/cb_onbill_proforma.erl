-module(cb_onbill_proforma).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1, content_types_provided/2
         ,validate/1, validate/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(CB_PROFORMA_INVOICE, <<"onbills/proforma_invoice">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_proforma">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_proforma">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_proforma">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_proforma">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET,?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET,?HTTP_DELETE].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbill_poforma(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_onbill_poforma(Context, Id, cb_context:req_verb(Context)).

-spec validate_onbill_poforma(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill_poforma(Context, ?HTTP_GET) ->
    summary(Context);
validate_onbill_poforma(Context, ?HTTP_PUT) ->
    create(Context).

validate_onbill_poforma(Context, <<Year:4/binary, Month:2/binary, _/binary>> = Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month)));
validate_onbill_poforma(Context0, <<Year:4/binary, Month:2/binary, _/binary>> = Id, ?HTTP_DELETE) ->
    Context = crossbar_doc:load(Id
                               ,cb_context:set_account_modb(Context0, kz_term:to_integer(Year), kz_term:to_integer(Month))
                               ,?TYPE_CHECK_OPTION(<<"onbill">>)
                               ),
    case cb_context:resp_status(Context) of
        'success' ->
            NewDoc = kz_json:set_value(<<"deleted_by_user">>, 'true', cb_context:doc(Context)),
            crossbar_doc:save(cb_context:set_doc(Context, NewDoc), ?TYPE_CHECK_OPTION(<<"onbill">>));
        _Status ->
            Context
   end.

-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    {Year, Month, _} = erlang:date(),
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    onbill_util:maybe_add_design_doc(Modb, <<"onbills">>),
    crossbar_doc:load_view(?CB_PROFORMA_INVOICE, []
                          ,cb_context:set_account_db(Context, Modb)
                          ,fun onbill_util:normalize_view_results/2).

-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    {Year, Month, _} = erlang:date(),
    ReqData = cb_context:req_data(Context),
    lager:info("IAM proforma ReqData: ~p",[ReqData]),
    AccountId = cb_context:account_id(Context),
    Amount = kz_json:get_number_value(<<"amount">>, ReqData),
    case onbill_docs:create_doc(Amount, AccountId, {[{<<"document_type">>,<<"proforma_invoice">>}]}) of
        {'ok', JObj} ->
            cb_context:set_resp_status(crossbar_doc:load(kz_doc:id(JObj)
                                                        ,cb_context:set_account_modb(Context
                                                                                    ,kz_term:to_integer(Year)
                                                                                    ,kz_term:to_integer(Month)
                                                                                    )
                                                        ,?TYPE_CHECK_OPTION(<<"onbill">>)
                                                        )
                                       ,'success');
        _ ->
            cb_context:add_system_error('error', Context)
    end.

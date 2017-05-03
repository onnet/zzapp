-module(cb_onbills).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-include("onbill.hrl").

-define(CB_LIST, <<"onbills/crossbar_listing">>).
-define(PRIOD_DOCS_VIEW, <<"onbills/docs_by_period_ts">>).
-define(ATTACHMENT, <<"attachment">>).
-define(GENERATE, <<"generate">>).
-define(CURRENT_SERVICES, <<"current_services">>).
-define(CURRENT_BILLING_PERIOD, <<"current_billing_period">>).
-define(BILLING_PERIODS, <<"billing_periods">>).
-define(PERIOD_BALANCE, <<"period_balance">>).
-define(CURRENCY_SIGN, <<"currency_sign">>).
-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                               %   ,{<<"text">>, <<"plain">>}
                                 ]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbills">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbills">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbills">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbills">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(?GENERATE) ->
    [?HTTP_PUT];
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_,?ATTACHMENT) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,?ATTACHMENT) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,?GENERATE) ->
    Context.
content_types_provided(Context,_,?ATTACHMENT) ->
    CTP = [{'to_binary', [{<<"application">>, <<"pdf">>}]}],
    cb_context:set_content_types_provided(Context, CTP).

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbill(Context, cb_context:req_verb(Context)).
validate(Context, ?CURRENT_SERVICES) ->
    validate_current_services(Context, cb_context:req_verb(Context));
validate(Context, ?CURRENT_BILLING_PERIOD) ->
    validate_current_billing_period(Context, cb_context:req_verb(Context));
validate(Context, ?BILLING_PERIODS) ->
    validate_billing_periods(Context, cb_context:req_verb(Context));
validate(Context, ?PERIOD_BALANCE) ->
    validate_period_balance(Context, cb_context:req_verb(Context));
validate(Context, ?CURRENCY_SIGN) ->
    validate_currency_sign(Context, cb_context:req_verb(Context));
validate(Context, ?GENERATE) ->
    lager:info("IAMGEN 1"),
    validate_generate(Context, cb_context:req_verb(Context)).
validate(Context, Id, ?ATTACHMENT) ->
    validate_onbill(Context, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_generate(cb_context:context(), http_method()) -> cb_context:context().
validate_generate(Context, ?HTTP_PUT) ->
    lager:info("IAMGEN 2"),
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    DocType = kz_json:get_value(<<"doc_type">>, ReqData),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, ReqData),
    validate_generate(Context, ?TO_BIN(DocType), AccountId, Timestamp).

validate_generate(Context, <<"calls_reports">>, AccountId, Timestamp) when is_integer(Timestamp) ->
    lager:info("IAMGEN 3"),
    generate_per_minute_reports(Context, AccountId, Timestamp);
validate_generate(Context, <<"transaction_invoice">>, AccountId, Timestamp) when is_integer(Timestamp) ->
    lager:info("IAMGEN 4"),
    generate_transaction_based_invoice(Context
                                      ,AccountId
                                      ,kz_json:get_value(<<"transaction_id">>, cb_context:req_data(Context))
                                      ,Timestamp);
validate_generate(Context, _, AccountId, Timestamp) when is_integer(Timestamp) ->
    lager:info("IAMGEN 5"),
    maybe_generate_billing_docs(Context, AccountId, Timestamp, 'generate_docs');
validate_generate(Context, _, _, _) ->
    lager:info("IAMGEN 6"),
    Message = <<"Period timestamp required">>,
    cb_context:add_validation_error(
      <<"Period timestamp">>
      ,<<"required">>
      ,kz_json:from_list([{<<"message">>, Message}])
      ,Context
     ).

generate_transaction_based_invoice(Context, AccountId, <<Year:4/binary, Month:2/binary, "-", _/binary>> = TransctionId, Timestamp) ->
    lager:info("IAMGEN 7"),
    {'ok', TransactionJobj} =
        kazoo_modb:open_doc(AccountId, TransctionId),
  lager:info("IAMGEN 7.2  TransactionJobj: ~p",[TransactionJobj]),
    Amount = kz_json:get_value(<<"pvt_amount">>, TransactionJobj),
  lager:info("IAMGEN 7.3  Amount: ~p",[Amount]),
    {{InvYear, InvMonth, InvDay}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    {'ok', DocNumber} =
        onbill_docs_numbering:maybe_get_new_number(AccountId
                                                  ,<<"transaction_based_invoice">>
                                                  ,InvYear
                                                  ,InvMonth),
            lager:info("IAMGEN 8 DocNumber: ~p",[DocNumber]),
    case onbill_docs:create_doc_by_type(Amount, AccountId, <<"transaction_based_invoice">>, DocNumber, InvYear, InvMonth, InvDay) of
        {'ok', JObj} ->
            lager:info("IAMGEN 9 JObj: ~p",[JObj]),
            InvoiceId = kz_doc:id(JObj),
            Values = [{[<<"metadata">>,<<"invoice_number">>], DocNumber}
                     ,{[<<"metadata">>,<<"invoice_id">>], InvoiceId}
                     ,{[<<"metadata">>,<<"invoice_timestamp">>], Timestamp}],
            {'ok', NewDoc} = kazoo_modb:save_doc(AccountId, kz_json:set_values(Values,TransactionJobj), ?TO_INT(Year), ?TO_INT(Month)),
            lager:info("IAMGEN 9 NewDoc: ~p",[NewDoc]),
            cb_context:set_resp_status(crossbar_doc:load(InvoiceId
                                                        ,cb_context:set_account_modb(Context
                                                                                    ,?TO_INT(Year)
                                                                                    ,?TO_INT(Month)
                                                                                    )
                                                        ,?TYPE_CHECK_OPTION(<<"onbill">>)
                                                        )
                                      ,'success');
        _ ->
            cb_context:add_system_error('error', Context)
    end.

maybe_generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName) ->
    case cb_context:is_superduper_admin(Context) of
        'true' ->
    lager:info("IAMGEN 11"),
            generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName);
        'false' ->
    lager:info("IAMGEN 12"),
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName) ->
    lager:info("IAMGEN 13"),
    ResellerId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
    lager:info("IAMGEN 14"),
            _ = onbill_docs:FunName(AccountId, ?TO_INT(PeriodTimestamp)),
            cb_context:set_resp_status(Context, 'success');
        'false' ->
    lager:info("IAMGEN 15"),
            cb_context:add_system_error('forbidden', Context)
    end.

generate_per_minute_reports(Context, AccountId, PeriodTimestamp) ->
    onbill_docs:per_minute_reports(AccountId, ?TO_INT(PeriodTimestamp)),
    cb_context:set_resp_status(Context, 'success').

-spec validate_onbill(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill(Context, ?HTTP_GET) ->
    onbills_modb_summary(Context).

validate_onbill(Context0, <<Year:4/binary, Month:2/binary, "-", _/binary>> = Id, ?ATTACHMENT, ?HTTP_GET) ->
    Context = crossbar_doc:load(Id, cb_context:set_account_modb(Context0, ?TO_INT(Year), ?TO_INT(Month))),
    case kz_doc:attachment_names(cb_context:doc(Context)) of
        [] -> 
            cb_context:add_system_error('no_attachment_found', Context);
        [AttachmentId|_] ->
            crossbar_doc:load_attachment(Id, AttachmentId, [], Context)
    end.

-spec onbills_modb_summary(cb_context:context()) -> cb_context:context().
onbills_modb_summary(Context) ->
    AccountId = cb_context:account_id(Context),
    ReqTs = case cb_context:req_value(Context, <<"timestamp">>) of
                'undefined' -> kz_time:current_tstamp();
                Ts -> Ts
            end,
    {SYear, SMonth, SDay} = onbill_util:period_start_date(AccountId, ?TO_INT(ReqTs)),
    {EYear, EMonth, EDay} = onbill_util:period_end_date(AccountId, ?TO_INT(ReqTs)),
    case SMonth of
        EMonth ->
            Modb = kazoo_modb:get_modb(AccountId, ?TO_INT(SYear), ?TO_INT(SMonth)),
            onbill_util:maybe_add_design_doc(Modb, <<"onbills">>),
            Context1 = cb_context:set_account_db(Context, Modb),
            crossbar_doc:load_view(?PRIOD_DOCS_VIEW, [], Context1, fun onbill_util:normalize_view_results/2);
        _ ->
            SViewOpts = [{'startkey', ?BEGIN_DAY_TS(SMonth, SYear, SDay)}
                        ,{'year', SYear}
                        ,{'month', SMonth}
                        ],
            EViewOpts = [{'endkey', ?END_DAY_TS(EMonth, EYear, EDay)}
                        ,{'year', EYear}
                        ,{'month', EMonth}
                        ],
            SRes = case kazoo_modb:get_results(AccountId, ?PRIOD_DOCS_VIEW, SViewOpts) of
                       {'ok', SJObjs} -> [kz_json:get_value(<<"value">>, JObj) || JObj <- SJObjs];
                       _ -> []
                   end,
            ERes = case kazoo_modb:get_results(AccountId, ?PRIOD_DOCS_VIEW, EViewOpts) of
                       {'ok', EJObjs} ->  [kz_json:get_value(<<"value">>, JObj) || JObj <- EJObjs];
                       _ -> []
                   end,
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, SRes ++ ERes}
                               ])
    end.
            
-spec validate_current_services(cb_context:context(), http_method()) -> cb_context:context().
validate_current_services(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    {'ok', Items} = kz_service_plans:create_items(ServicesJObj),
    ItemsList = onbill_bk_util:select_non_zero_items_list(Items, AccountId),
    ItemsCalculatedList = [onbill_bk_util:calc_item(ItemJObj, AccountId) || ItemJObj <- ItemsList],
    CurrentServicesJObj =
        kz_json:from_list([{<<"total_amount">>, onbill_bk_util:items_amount(ItemsList, AccountId, 0.0)}
                          ,{<<"services_list">>, ItemsCalculatedList}
                          ,{<<"account_id">>, AccountId}
                          ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, CurrentServicesJObj}
                       ]);
validate_current_services(Context, _) ->
    Context.

-spec validate_currency_sign(cb_context:context(), http_method()) -> cb_context:context().
validate_currency_sign(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    Vars =
        case kz_services:is_reseller(AccountId) of
            'true' -> onbill_util:account_vars(AccountId);
            'false' -> onbill_util:reseller_vars(AccountId)
        end,
    JObj =
        kz_json:from_list([{<<"currency_sign">>, kz_json:get_value(<<"currency_sign">>, Vars)}
                          ,{<<"account_id">>, AccountId}
                          ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObj}
                       ]);
validate_currency_sign(Context, _) ->
    Context.

-spec validate_current_billing_period(cb_context:context(), http_method()) -> cb_context:context().
validate_current_billing_period(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    {Year, Month, Day} = onbill_util:period_start_date(AccountId),
    {EYear, EMonth, EDay} = onbill_util:period_end_date(AccountId, Year, Month, Day),
    JObj =
        kz_json:from_list([{<<"account_id">>, AccountId}
                          ,{<<"billing_day">>, onbill_util:billing_day(AccountId)}
                          ,{<<"period_start">>, onbill_util:date_json(Year, Month, Day)}
                          ,{<<"period_end">>, onbill_util:date_json(EYear, EMonth, EDay)}
                          ]),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObj}
                       ]);
validate_current_billing_period(Context, _) ->
    Context.

-spec validate_billing_periods(cb_context:context(), http_method()) -> cb_context:context().
validate_billing_periods(Context, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    JObjs = onbill_util:list_account_periods(AccountId),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, JObjs}
                       ]);
validate_billing_periods(Context, _) ->
    Context.

-spec validate_period_balance(cb_context:context(), http_method()) -> cb_context:context().
validate_period_balance(Context, ?HTTP_GET) ->
    case cb_context:req_value(Context, <<"timestamp">>) of
        'undefined' ->
            Context;
        PeriodTS -> 
            AccountId = cb_context:account_id(Context),
            {SYear, SMonth, SDay} = onbill_util:period_start_date(AccountId, ?TO_INT(PeriodTS)),
            OpeningBalance = onbill_util:day_start_balance_dollars(AccountId, SYear, SMonth, SDay),
            {NYear, NMonth, NDay} = onbill_util:next_period_start_date(AccountId, SYear, SMonth, SDay),
            ClosingBalance = onbill_util:day_start_balance_dollars(AccountId, NYear, NMonth, NDay),
            Balances = [{<<"opening_balance">>, OpeningBalance}
                       ,{<<"closing_balance">>, ClosingBalance}
                       ,{<<"account_id">>, AccountId}
                       ,{<<"period_start">>, onbill_util:date_json(SYear, SMonth, SDay)}
                       ,{<<"period_end">>
                        ,onbill_util:date_json(onbill_util:period_end_date(AccountId, ?TO_INT(PeriodTS)))
                        }
                       ],
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, kz_json:from_list(Balances)}
                               ])
    end;
validate_period_balance(Context, _) ->
    Context.

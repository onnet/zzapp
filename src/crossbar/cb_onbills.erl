-module(cb_onbills).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/3
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
-define(ALL_CHILDREN, <<"all_children">>).
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
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(?GENERATE,_) ->
    [?HTTP_PUT];
allowed_methods(_,?ATTACHMENT) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?GENERATE,_) -> 'true';
resource_exists(_,?ATTACHMENT) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,?GENERATE,_) ->
    Context;
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
    validate_currency_sign(Context, cb_context:req_verb(Context)).
validate(Context, ?GENERATE, Id) ->
    validate_generate(Context, Id, cb_context:req_verb(Context));
validate(Context, Id, ?ATTACHMENT) ->
    validate_onbill(Context, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_generate(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_generate(Context, AccountId, ?HTTP_PUT) ->
    PeriodTimestamp = kz_json:get_integer_value(<<"period_timestamp">>, cb_context:req_data(Context)),
    validate_generate_ts(Context, AccountId, PeriodTimestamp).

validate_generate_ts(Context, AccountId, PeriodTimestamp) when is_integer(PeriodTimestamp) ->
    case kz_json:get_value(<<"doc_type">>, cb_context:req_data(Context)) of
        "calls_reports" -> generate_per_minute_reports(Context, AccountId, PeriodTimestamp);
        _ -> maybe_generate_billing_docs(Context, AccountId, PeriodTimestamp, 'generate_docs')
    end;

validate_generate_ts(Context, _, _) ->
    Message = <<"Period timestamp required">>,
    cb_context:add_validation_error(
      <<"Period timestamp">>
      ,<<"required">>
      ,kz_json:from_list([{<<"message">>, Message}])
      ,Context
     ).

maybe_generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName) ->
    case cb_context:is_superduper_admin(Context) of
        'true' ->
            generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

maybe_spawn_generate_billing_docs(AccountId, PeriodTimestamp, FunName, N) ->
    case onbill_util:is_billable(AccountId) of
        'true' ->
            spawn(fun() ->
                      timer:sleep(N * 2000),
                      docs:FunName(AccountId, kz_term:to_integer(PeriodTimestamp))
                  end),
            N+1;
        'false' -> N
    end.

generate_billing_docs(Context, ?ALL_CHILDREN, PeriodTimestamp, FunName) ->
    ResellerId = cb_context:account_id(Context),
    case onbill_util:get_children_list(ResellerId) of
        {'ok', Accounts} ->
            _ = lists:foldl(fun(X, N) ->
                                maybe_spawn_generate_billing_docs(kz_json:get_value(<<"id">>, X), PeriodTimestamp, FunName, N)
                            end
                           ,1
                           , Accounts),
            cb_context:set_resp_status(Context, 'success');
        {'error', _Reason} ->
            cb_context:add_system_error('error', Context)
    end;
generate_billing_docs(Context, AccountId, PeriodTimestamp, FunName) ->
    ResellerId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
            _ = docs:FunName(AccountId, kz_term:to_integer(PeriodTimestamp)),
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

generate_per_minute_reports(Context, AccountId, PeriodTimestamp) ->
    docs:per_minute_reports(AccountId, kz_term:to_integer(PeriodTimestamp)),
    cb_context:set_resp_status(Context, 'success').

-spec validate_onbill(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill(Context, ?HTTP_GET) ->
    onbills_modb_summary(Context).

validate_onbill(Context0, <<Year:4/binary, Month:2/binary, "-", _/binary>> = Id, ?ATTACHMENT, ?HTTP_GET) ->
    Context = crossbar_doc:load(Id, cb_context:set_account_modb(Context0, kz_term:to_integer(Year), kz_term:to_integer(Month))),
    case kz_doc:attachment_names(cb_context:doc(Context)) of
        [] -> 
            cb_context:add_system_error('no_attachment_found', Context);
        [AttachmentId|_] ->
            crossbar_doc:load_attachment(Id, AttachmentId, [], Context)
    end.

-spec onbills_modb_summary(cb_context:context()) -> cb_context:context().
onbills_modb_summary(Context) ->
    AccountId = cb_context:account_id(Context),
    ReqTs = case cb_context:req_value(Context, <<"period_timestamp">>) of
                'undefined' -> kz_time:current_tstamp();
                Ts -> Ts
            end,
    {SYear, SMonth, SDay} = onbill_util:period_start_date(AccountId, kz_term:to_integer(ReqTs)),
    {EYear, EMonth, EDay} = onbill_util:period_last_day_by_first_one(SYear, SMonth, SDay),
    case SMonth of
        EMonth ->
            Modb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(SYear), kz_term:to_integer(SMonth)),
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
    {EYear, EMonth, EDay} = onbill_util:period_last_day_by_first_one(Year, Month, Day),
    JObj =
        kz_json:from_list([{<<"account_id">>, AccountId}
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
    case cb_context:req_value(Context, <<"period_timestamp">>) of
        'undefined' ->
            Context;
        PeriodTS -> 
            AccountId = cb_context:account_id(Context),
            {SYear, SMonth, SDay} = onbill_util:period_start_date(AccountId, kz_term:to_integer(PeriodTS)),
            OpeningBalance = onbill_util:day_start_balance_dollars(AccountId, SYear, SMonth, SDay),
            {NYear, NMonth, NDay} = onbill_util:next_period_start_date(AccountId, SYear, SMonth, SDay),
            ClosingBalance = onbill_util:day_start_balance_dollars(AccountId, NYear, NMonth, NDay),
            Balances = [{<<"opening_balance">>, OpeningBalance}
                       ,{<<"closing_balance">>, ClosingBalance}
                       ,{<<"account_id">>, AccountId}
                       ,{<<"period_start">>, onbill_util:date_json(SYear, SMonth, SDay)}
                       ,{<<"period_end">>, onbill_util:date_json(onbill_util:period_last_day_by_first_one(SYear, SMonth, SDay))}
                       ],
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, kz_json:from_list(Balances)}
                               ])
    end;
validate_period_balance(Context, _) ->
    Context.

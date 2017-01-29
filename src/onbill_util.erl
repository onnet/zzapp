-module(onbill_util).

-export([check_db/1
        ,maybe_add_design_doc/2
        ,get_attachment/2
        ,price_round/1
        ,account_carriers_list/1
        ,account_vars/1
        ,carrier_doc/2
        ,reseller_vars/1
        ,reseller_country_of_residence/1
        ,maybe_main_carrier/2
        ,get_main_carrier/2
        ,format_datetime/1
        ,is_billable/1
        ,validate_relationship/2
        ,get_children_list/1
        ,normalize_view_results/2
        ,normalize_view_active_results/2
        ,maybe_fee_active/2
        ,next_month/2
        ,adjust_period_first_day/3
        ,adjust_period_last_day/3
        ,days_in_period/3
        ,days_left_in_period/4
        ,period_last_day_by_first_one/3
        ,period_end_modb_by_start/4
        ,period_start_tuple/3
        ,period_end_tuple_by_start/3
        ,period_tuple/3
        ,period_start_date/1
        ,period_start_date/2
        ,period_start_date/4
        ,get_account_created_date/1
        ,maybe_allow_postpay/1
        ,is_trial_account/1
        ,maybe_administratively_convicted/1
        ,maybe_convicted/1
        ,is_service_plan_assigned/1
        ,ensure_service_plan/1
        ,replicate_account_doc/1
        ,transit_to_full_suscription_state/1
        ,reconcile_and_maybe_sync/1
        ]).

-include("onbill.hrl").

-define(KEY_TRIAL_EXPIRATION, <<"pvt_trial_expires">>).

-spec check_db(ne_binary()) -> 'ok'.
check_db(Db) when is_binary(Db) ->
    do_check_db(Db, kz_datamgr:db_exists(Db)).

-spec do_check_db(ne_binary(), boolean()) -> 'ok'.
do_check_db(_Db, 'true') -> 'ok';
do_check_db(Db, 'false') ->
    lager:debug("create Db ~p", [Db]),
    _ = kz_datamgr:db_create(Db).

-spec maybe_add_design_doc(ne_binary(), ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_design_doc(DbName, ViewName) ->
    case kz_datamgr:lookup_doc_rev(DbName, <<"_design/", ViewName/binary>>) of
        {'error', 'not_found'} ->
            lager:warning("adding onbill views to db: ~s", [DbName]),
            kz_datamgr:revise_doc_from_file(DbName
                                           ,'onbill'
                                           ,<<"views/", ViewName/binary, ".json">>
                                          );
        {'ok', _ } -> 'ok'
    end.

-spec get_attachment(ne_binary(), ne_binary()) -> ok.
get_attachment(AttachmentId, Db) ->
    case kz_datamgr:fetch_attachment(Db, AttachmentId, <<(kz_util:to_binary(AttachmentId))/binary, ".pdf">>) of
        {'ok', _} = OK -> OK;
        E -> E
    end.

-spec price_round(number()) -> number().
price_round(Price) ->
    round(Price * 100) / 100.

-spec account_carriers_list(ne_binary()) -> list().
account_carriers_list(AccountId) ->
    kz_json:get_value(<<"carriers">>, reseller_vars(AccountId), []).

-spec carrier_doc(ne_binary(), ne_binary()) -> any().
carrier_doc(Carrier, AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = kz_util:format_account_id(ResellerId,'encoded'),
    {'ok', CarrierDoc} =  kz_datamgr:open_doc(DbName, ?CARRIER_DOC(Carrier)),
    CarrierDoc.

-spec account_vars(ne_binary()) -> list().
account_vars(AccountId) ->
    DbName = kz_util:format_account_id(AccountId,'encoded'),
    case kz_datamgr:open_doc(DbName, ?ONBILL_DOC) of
        {'ok', OnbillDoc} ->
            OnbillDoc;
        _ ->
            lager:info("can't open onbill doc in ~p, please check if it exists",[DbName]),
            kz_json:new()
    end.

-spec reseller_vars(ne_binary()) -> proplist().
reseller_vars(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    account_vars(ResellerId).

-spec reseller_country_of_residence(ne_binary()) -> proplist().
reseller_country_of_residence(AccountId) ->
    kz_util:to_lower_binary(kz_json:get_value(<<"iso_code_country_of_residence">>, reseller_vars(AccountId), <<"uk">>)).

-spec maybe_main_carrier(ne_binary(), kz_json:object()) -> boolean(). 
maybe_main_carrier(Carrier, AccountId) when is_binary(Carrier) ->
    maybe_main_carrier(carrier_doc(Carrier, AccountId), AccountId);
maybe_main_carrier(CarrierDoc, _) ->
    case kz_json:get_value(<<"carrier_type">>, CarrierDoc) of
        <<"main">> -> 'true';
        _ -> 'false'
    end.

-spec get_main_carrier(ne_binary()|list(), ne_binary()) -> ne_binary().
get_main_carrier([Carrier],_) ->
    Carrier;
get_main_carrier([Carrier|T], AccountId) ->
    case maybe_main_carrier(Carrier, AccountId) of
        'true' -> Carrier;
        _ -> get_main_carrier(T, AccountId)
    end;
get_main_carrier(AccountId, _) ->
    get_main_carrier(account_carriers_list(AccountId), AccountId).

-spec format_datetime(integer()) -> ne_binary().
format_datetime(TStamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(TStamp),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    kz_util:to_binary(StrTime).

-spec is_billable(ne_binary()) -> boolean().
is_billable(AccountId) ->
    {'ok', JObj} = kz_account:fetch(AccountId),
    kz_account:is_enabled(JObj).

-spec validate_relationship(ne_binary(), ne_binary()) -> boolean().
validate_relationship(ChildId, ResellerId) ->
    case get_children_list(ResellerId) of
        {'ok', Accounts} ->
            AccountIds = lists:map(fun(Account) -> kz_json:get_value(<<"id">>, Account) end, Accounts),
            lists:member(ChildId, AccountIds);
        {'error', _Reason} = E ->
            lager:info("failed to load children. error: ~p", [E]),
            'false'
    end.

-spec get_children_list(ne_binary()) -> {'ok', proplist()} | {'error', any()}.
get_children_list(ResellerId) ->
    ViewOpts = [{'startkey', [ResellerId]}
               ,{'endkey', [ResellerId, kz_json:new()]}
               ],
    kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_view_active_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_active_results(JObj, Acc) ->
    case maybe_fee_active(kz_util:current_tstamp(), JObj) of
        'true' ->
            [kz_json:get_value(<<"value">>, JObj)|Acc];
        'false' ->
            Acc
    end.

-spec maybe_fee_active(integer(), kz_json:object()) -> boolean().
maybe_fee_active(LookupTstamp, Fee) ->
    LookupTstamp > kz_json:get_value([<<"value">>, <<"service_starts">>], Fee)
    andalso
    LookupTstamp < kz_json:get_value([<<"value">>, <<"service_ends">>], Fee).

-spec prev_month(kz_year(), kz_month()) -> {kz_year(), kz_month()}.
prev_month(Year, 1) ->
    {Year - 1, 12};
prev_month(Year, Month) ->
    {Year, Month - 1}.

-spec next_month(kz_year(), kz_month()) -> {kz_year(), kz_month()}.
next_month(Year, 12) ->
    {Year + 1, 1};
next_month(Year, Month) ->
    {Year, Month + 1}.

-spec adjust_period_first_day(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
adjust_period_first_day(Year, Month, Day) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Year, Month),
    case (Day > LastDayOfMonth) of
        'true' ->
            {NYear, NMonth} = next_month(Year, Month),
            {NYear, NMonth, 1};
        'false' ->
            {Year, Month, Day}
    end.

-spec adjust_period_last_day(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
adjust_period_last_day(Year, Month, Day) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Year, Month),
    case (Day > LastDayOfMonth) of
        'true' ->
            {Year, Month, LastDayOfMonth};
        'false' ->
            {Year, Month, Day}
    end.

-spec period_last_day_by_first_one(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
period_last_day_by_first_one(Year, Month, 1) -> 
    {Year, Month, calendar:last_day_of_the_month(Year, Month)};
period_last_day_by_first_one(Year, Month, Day) -> 
    {FY, FM, FD} = adjust_period_first_day(Year, Month, Day),
    {NextMonthYear, NextMonth} = next_month(FY, FM),
    adjust_period_last_day(NextMonthYear, NextMonth, FD - 1).

-spec days_in_period(kz_year(), kz_month(), kz_day()) -> integer().
days_in_period(StartYear, StartMonth, StartDay) ->
  %  {NextMonthYear, NextMonth} = next_month(StartYear, StartMonth),
  %  calendar:date_to_gregorian_days(adjust_period_last_day(NextMonthYear, NextMonth, StartDay))
  %  -
  %  calendar:date_to_gregorian_days(adjust_period_first_day(StartYear, StartMonth, StartDay)).
  {Year, Month, _} = adjust_period_first_day(StartYear, StartMonth, StartDay),
  calendar:last_day_of_the_month(Year, Month).

-spec days_left_in_period(kz_year(), kz_month(), kz_day(), gregorian_seconds()) -> integer().
days_left_in_period(StartYear, StartMonth, StartDay, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    {NextMonthYear, NextMonth} = next_month(StartYear, StartMonth),
    calendar:date_to_gregorian_days(adjust_period_last_day(NextMonthYear, NextMonth, StartDay))
    -
    calendar:date_to_gregorian_days(Year, Month, Day).

-spec period_start_tuple(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
period_start_tuple(Year, Month, Day) ->
    {Y, M, D} = onbill_util:adjust_period_first_day(Year, Month, Day),
    period_tuple(Y, M, D).

-spec period_end_modb_by_start(ne_binary(), kz_year(), kz_month(), kz_day()) -> ne_binary().
period_end_modb_by_start(AccountId, Year, Month, Day) ->
    {SY, SM, SD} = onbill_util:adjust_period_first_day(Year, Month, Day),
    {Y, M, _} = onbill_util:period_last_day_by_first_one(SY, SM, SD),
    kazoo_modb:get_modb(AccountId, Y, M).

-spec period_end_tuple_by_start(kz_year(), kz_month(), kz_day()) -> proplist().
period_end_tuple_by_start(Year, Month, Day) ->
    {SY, SM, SD} = onbill_util:adjust_period_first_day(Year, Month, Day),
    {Y, M, D} = onbill_util:period_last_day_by_first_one(SY, SM, SD),
    period_tuple(Y, M, D).

-spec period_tuple(kz_year(), kz_month(), kz_day()) -> proplist().
period_tuple(Year, Month, Day) when is_integer(Day) ->
    period_tuple(Year, Month, ?TO_BIN(Day));
period_tuple(Year, Month, Day) ->
    [{<<"year">>, ?TO_BIN(Year)}
    ,{<<"month_short">>, ?TO_BIN(httpd_util:month(?TO_INT(Month)))}
    ,{<<"month_pad">>, ?TO_BIN(kz_util:pad_month(Month))}
    ,{<<"day">>, Day}
    ].

-spec period_start_date(ne_binary()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId) ->
    Timestamp = kz_util:current_tstamp(),
    period_start_date(AccountId, Timestamp).

-spec period_start_date(ne_binary(), gregorian_seconds()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    period_start_date(AccountId, Year, Month, Day).

-spec period_start_date(ne_binary(), kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId, Year, Month, Day) ->
    case kz_json:get_value(<<"pvt_billing_day">>, account_vars(AccountId)) of
        'undefined' ->
            _ = set_billing_day(AccountId),
            period_start_date(AccountId, Year, Month, Day);
        BillingDay ->
            BDay = kz_util:to_integer(BillingDay),
            case Day >= BDay of
                'true' ->
                     {Year, Month, BDay};
                'false' ->
                     {PrevYear, PrevMonth} = prev_month(Year, Month),
                     {PrevYear, PrevMonth, BDay}
            end
    end.

set_billing_day(AccountId) ->
    BillingDay =
        case kz_json:get_value(<<"pvt_billing_period_type">>, account_vars(AccountId)) of
            <<"calendar_month">> -> 1;
            _ ->
                {{_,_,Today},_} = calendar:universal_time(),
                Today
        end,
    set_billing_day(BillingDay, AccountId).

set_billing_day(BillingDay, AccountId) ->
    DbName = kz_util:format_account_id(AccountId,'encoded'),
    NewDoc = case kz_datamgr:open_doc(DbName, ?ONBILL_DOC) of
        {ok, Doc} ->
            kz_json:set_value(<<"pvt_billing_day">>, BillingDay, Doc);
        {'error', 'not_found'} ->
            kz_json:set_values([{<<"_id">>, ?ONBILL_DOC}
                               ,{<<"pvt_type">>, ?ONBILL_DOC}
                               ,{<<"pvt_billing_day">>, BillingDay}
                               ]
                              ,kz_json:new())
    end,
    kz_datamgr:ensure_saved(DbName, NewDoc).

-spec get_account_created_date(ne_binary()) -> {kz_year(), kz_month(), kz_day()}.
get_account_created_date(AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    Timestamp = kz_json:get_value(<<"pvt_created">>, AccountDoc),
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    {Year, Month, Day}.

-spec maybe_allow_postpay(ne_binary()) -> boolean().
maybe_allow_postpay(AccountId) ->
    Limits = j5_limits:get(AccountId),
    case j5_limits:allow_postpay(Limits) of
        'false' -> 'false';
        'true' -> {'true', j5_limits:max_postpay(Limits)}
    end.

-spec is_trial_account(ne_binary()) -> boolean().
is_trial_account(AccountId) ->
    {'ok', JObj} = kz_account:fetch(AccountId),
    case kz_account:trial_expiration(JObj) of
        'undefined' -> 'false';
        _ -> 'true'
    end.

-spec maybe_convicted(ne_binary()) -> 'ok'|'delinquent'.
maybe_convicted(AccountId) ->
    Balance = wht_util:current_balance(AccountId),
    case onbill_util:maybe_allow_postpay(AccountId) of
        'false' when Balance < 0 -> 'true';
        'false' -> 'false';
        {'true', MaxPostpay} when Balance < MaxPostpay ->  'true';
        {'true', _} -> 'false'
    end.

-spec maybe_administratively_convicted(ne_binary()) -> boolean().
maybe_administratively_convicted(AccountId) ->
    {ok, ServicesJObj} = kz_services:fetch_services_doc(AccountId,'false'),
    case kzd_services:status(ServicesJObj) of
        <<"good_standing">> -> 'false';
        _ ->
            case kz_json:get_value(<<"pvt_status_reason">>, ServicesJObj) of
                <<"administratively_convicted">> -> 'true';
                _ -> 'false'
            end
    end.

-spec is_service_plan_assigned(ne_binary()) -> boolean().
is_service_plan_assigned(AccountId) ->
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    Plans = kz_service_plans:plan_summary(ServicesJObj),
    not kz_util:is_empty(Plans).

-spec ensure_service_plan(ne_binary()) -> 'ok'.
ensure_service_plan(AccountId) ->
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    case is_service_plan_assigned(AccountId) of
        'false' when MasterAccount == AccountId ->
            'ok';
        'false' ->
            _ = add_service_plan(AccountId);
        'true' ->
            'ok'
    end.

add_service_plan(AccountId) ->
    case default_service_plan(AccountId) of
        'undefined' ->
            lager:info("no default service plan found to apply to account: ~p",[AccountId]);
        DefaultPlan -> 
            add_service_plan(DefaultPlan, AccountId)
    end.

add_service_plan(PlanId, AccountId) ->
    Services = kz_services:fetch(AccountId),
    kz_services:save(kz_services:add_service_plan(PlanId, Services)).

-spec default_service_plan(ne_binary()) -> ne_binary().
default_service_plan(AccountId) ->
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    kz_json:get_value(<<"default_service_plan">>
                     ,reseller_vars(AccountId)
                     ,kz_json:get_value(<<"default_service_plan">>,reseller_vars(MasterAccount))
                     ).

-spec transit_to_full_suscription_state(ne_binary()) -> 'ok'.
transit_to_full_suscription_state(AccountId) ->
    set_billing_day(AccountId),
    {'ok', Doc} = kz_account:fetch(AccountId),
    NewDoc = kz_json:delete_key(?KEY_TRIAL_EXPIRATION, Doc),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', JObj} = kz_datamgr:ensure_saved(AccountDb, NewDoc),
    replicate_account_doc(JObj),
    {'ok', JObj}.

-spec replicate_account_doc(kz_json:object()) ->
                                          {'ok', kz_json:object()} |
                                          {'error', any()}.
replicate_account_doc(JObj) ->
    AccountId = kz_doc:id(JObj),
    case kz_datamgr:lookup_doc_rev(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, kz_doc:set_revision(JObj, Rev));
        _Else ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, kz_doc:delete_revision(JObj))
    end.

-spec reconcile_and_maybe_sync(ne_binary()) -> any().
reconcile_and_maybe_sync(AccountId) ->
    Services = kz_services:reconcile(AccountId),
    case kz_services:is_dirty(Services) of
        'true' ->
            kz_service_sync:sync(AccountId);
        'false' ->
            'ok'
    end.

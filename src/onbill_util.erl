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
        ,adjust_period_first_day/1
        ,adjust_period_first_day/3
        ,adjust_period_last_day/1
        ,adjust_period_last_day/3
        ,days_in_period/2
        ,days_in_period/4
        ,days_left_in_period/2
        ,period_end_modb/4
        ,date_json/1
        ,date_json/3
        ,period_start_date/1
        ,period_start_date/2
        ,period_start_date/4
        ,next_period_start_date/2
        ,next_period_start_date/4
        ,previous_period_start_date/2
        ,previous_period_start_date/4
        ,period_end_date/1
        ,period_end_date/2
        ,period_end_date/4
        ,account_creation_date/1
        ,account_creation_ts/1
        ,billing_day/1
        ,set_billing_day/2
        ,maybe_allow_postpay/1
        ,trial_has_expired/1
        ,is_trial_account/1
        ,maybe_administratively_convicted/1
        ,current_service_status/1
        ,maybe_convicted/1
        ,is_service_plan_assigned/1
        ,ensure_service_plan/1
        ,replicate_account_doc/1
        ,transit_to_full_suscription_state/1
        ,reconcile_and_maybe_sync/1
        ,maybe_save_as_dirty/1
        ,current_balance/1
        ,current_account_dollars/1
        ,maybe_process_new_billing_period/1
        ,list_account_periods/1
        ,period_openning_balance/4
        ,period_openning_balance_dollars/4
        ,day_start_balance/4
        ,day_start_balance_dollars/4
        ,get_range/3
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
    case kz_datamgr:fetch_attachment(Db, AttachmentId, <<(kz_term:to_binary(AttachmentId))/binary, ".pdf">>) of
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

-spec account_vars(ne_binary()) -> kz_json:object().
account_vars(AccountId) ->
    DbName = kz_util:format_account_id(AccountId,'encoded'),
    case kz_datamgr:open_doc(DbName, ?ONBILL_DOC) of
        {'ok', OnbillDoc} ->
            OnbillDoc;
        _ ->
            lager:info("can't open onbill doc in ~p, please check if it exists",[DbName]),
            kz_json:new()
    end.

-spec reseller_vars(ne_binary()) -> kz_json:object().
reseller_vars(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    account_vars(ResellerId).

-spec reseller_country_of_residence(ne_binary()) -> kz_proplist().
reseller_country_of_residence(AccountId) ->
    kz_term:to_lower_binary(kz_json:get_value(<<"iso_code_country_of_residence">>, reseller_vars(AccountId), <<"uk">>)).

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
    kz_term:to_binary(StrTime).

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

-spec get_children_list(ne_binary()) -> {'ok', kz_proplist()} | {'error', any()}.
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
    case maybe_fee_active(kz_time:current_tstamp(), JObj) of
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

-spec adjust_period_first_day({kz_year(), kz_month(), kz_day()}) -> {kz_year(), kz_month(), kz_day()}.
-spec adjust_period_first_day(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
adjust_period_first_day({Year, Month, Day}) ->
    adjust_period_first_day(Year, Month, Day).

adjust_period_first_day(Year, Month, 0) ->
    adjust_period_first_day(Year, Month, 1);
adjust_period_first_day(Year, Month, Day) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Year, Month),
    case (Day > LastDayOfMonth) of
        'true' ->
            {NYear, NMonth} = next_month(Year, Month),
            {NYear, NMonth, 1};
        'false' ->
            {Year, Month, Day}
    end.

-spec adjust_period_last_day({kz_year(), kz_month(), kz_day()}) -> {kz_year(), kz_month(), kz_day()}.
-spec adjust_period_last_day(kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
adjust_period_last_day({Year, Month, Day}) ->
    adjust_period_last_day(Year, Month, Day).

adjust_period_last_day(Year, Month, 0) ->
    {PYear, PMonth} = prev_month(Year, Month),
    {PYear, PMonth, calendar:last_day_of_the_month(PYear, PMonth)};
adjust_period_last_day(Year, Month, Day) ->
    LastDayOfMonth = calendar:last_day_of_the_month(Year, Month),
    case (Day > LastDayOfMonth) of
        'true' ->
            {Year, Month, LastDayOfMonth};
        'false' ->
            {Year, Month, Day}
    end.

-spec days_in_period(ne_binary(), integer()) -> integer().
-spec days_in_period(ne_binary(), kz_year(), kz_month(), kz_day()) -> integer().
days_in_period(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    days_in_period(AccountId, Year, Month, Day).
days_in_period(AccountId, Year, Month, Day) ->
    {SYear, SMonth, _} = period_start_date(AccountId, Year, Month, Day),
    calendar:last_day_of_the_month(SYear, SMonth).

-spec days_left_in_period(ne_binary(), gregorian_seconds()) -> integer().
days_left_in_period(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    calendar:date_to_gregorian_days(period_end_date(AccountId, Timestamp))
    -
    calendar:date_to_gregorian_days(Year, Month, Day) + 1.

-spec period_end_modb(ne_binary(), kz_year(), kz_month(), kz_day()) -> ne_binary().
period_end_modb(AccountId, Year, Month, Day) ->
    {Y, M, _} = period_end_date(AccountId, Year, Month, Day),
    kazoo_modb:get_modb(AccountId, Y, M).

-spec date_json({kz_year(), kz_month(), kz_day()}) -> kz_proplist().
-spec date_json(kz_year(), kz_month(), kz_day()) -> kz_proplist().
date_json({Year, Month, Day}) ->
    date_json(Year, Month, Day).

date_json(Year, Month, Day) when is_integer(Day) ->
    date_json(Year, Month, ?TO_BIN(Day));
date_json(Year, Month, Day) ->
    kz_json:from_list([{<<"year">>, ?TO_BIN(Year)}
                      ,{<<"month_short">>, ?TO_BIN(httpd_util:month(?TO_INT(Month)))}
                      ,{<<"month_pad">>, ?TO_BIN(kz_time:pad_month(Month))}
                      ,{<<"day">>, Day}
                      ,{<<"day_begins_ts">>, calendar:datetime_to_gregorian_seconds({{?TO_INT(Year), ?TO_INT(Month), ?TO_INT(Day)}, {0,0,0}})}
                      ,{<<"day_ends_ts">>, calendar:datetime_to_gregorian_seconds({{?TO_INT(Year), ?TO_INT(Month), ?TO_INT(Day)}, {23,59,59}})}
                      ]).

-spec period_start_date(ne_binary()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId) ->
    Timestamp = kz_time:current_tstamp(),
    period_start_date(AccountId, Timestamp).

-spec period_start_date(ne_binary(), gregorian_seconds()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    period_start_date(AccountId, Year, Month, Day).

-spec period_start_date(ne_binary(), kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
period_start_date(AccountId, Year, Month, Day) ->
    BDay = kz_term:to_integer(billing_day(AccountId)),
    case Day >= BDay of
        'true' ->
             adjust_period_first_day(Year, Month, BDay);
        'false' ->
             {PrevYear, PrevMonth} = prev_month(Year, Month),
             adjust_period_first_day(PrevYear, PrevMonth, BDay)
    end.

-spec period_end_date(ne_binary()) -> {kz_year(), kz_month(), kz_day()}.
period_end_date(AccountId) ->
    Timestamp = kz_time:current_tstamp(),
    period_end_date(AccountId, Timestamp).

-spec period_end_date(ne_binary(), gregorian_seconds()) -> {kz_year(), kz_month(), kz_day()}.
period_end_date(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    period_end_date(AccountId, Year, Month, Day).

-spec period_end_date(ne_binary(), kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
period_end_date(AccountId, Year, Month, Day) ->
    BDay = kz_term:to_integer(billing_day(AccountId)),
    case Day < BDay of
        'true' ->
             adjust_period_last_day(Year, Month, BDay-1);
        'false' ->
             {NextYear, NextMonth} = next_month(Year, Month),
             adjust_period_last_day(NextYear, NextMonth, BDay-1)
    end.

-spec next_period_start_date(ne_binary(), gregorian_seconds()) -> {kz_year(), kz_month(), kz_day()}.
-spec next_period_start_date(ne_binary(), kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
next_period_start_date(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    next_period_start_date(AccountId, Year, Month, Day).

next_period_start_date(AccountId, Year, Month, Day) ->
    {SYear, SMonth, SDay} = period_start_date(AccountId, Year, Month, Day),
    {NextMonthYear, NextMonth} = next_month(SYear, SMonth),
    adjust_period_first_day(NextMonthYear, NextMonth, SDay).

-spec previous_period_start_date(ne_binary(), gregorian_seconds()) -> {kz_year(), kz_month(), kz_day()}.
-spec previous_period_start_date(ne_binary(), kz_year(), kz_month(), kz_day()) -> {kz_year(), kz_month(), kz_day()}.
previous_period_start_date(AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    previous_period_start_date(AccountId, Year, Month, Day).

previous_period_start_date(AccountId, Year, Month, Day) ->
    {SYear, SMonth, SDay} = period_start_date(AccountId, Year, Month, Day),
    {PrevMonthYear, PrevMonth} = prev_month(SYear, SMonth),
    adjust_period_first_day(PrevMonthYear, PrevMonth, SDay).

-spec maybe_force_postpay_billing_day(ne_binary()) -> boolean().
maybe_force_postpay_billing_day(AccountId) ->
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    kz_json:get_atom_value(<<"force_postpay_billing_day">>
                          ,reseller_vars(AccountId)
                          ,kz_json:get_atom_value(<<"force_postpay_billing_day">>,reseller_vars(MasterAccount),'false')
                          ).

-spec maybe_force_prepay_billing_day(ne_binary()) -> boolean().
maybe_force_prepay_billing_day(AccountId) ->
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    kz_json:get_atom_value(<<"force_prepay_billing_day">>
                          ,reseller_vars(AccountId)
                          ,kz_json:get_atom_value(<<"force_prepay_billing_day">>,reseller_vars(MasterAccount),'false')
                          ).

-spec billing_day(ne_binary()) -> integer() | 'undefined'.
billing_day(AccountId) when is_binary(AccountId) ->
    case maybe_allow_postpay(AccountId) of
        {'true', _} ->
            case maybe_force_postpay_billing_day(AccountId) of
                'true' -> 1;
                'false' ->
                    billing_day(account_vars(AccountId), AccountId)
            end;
        'false' ->
            case maybe_force_prepay_billing_day(AccountId) of
                'true' -> 1;
                'false' ->
                    billing_day(account_vars(AccountId), AccountId)
            end
    end.

-spec billing_day(kz_json:object(), ne_binary()) -> integer() | 'undefined'.
billing_day(AccountVarsJObj, AccountId) ->
    case kz_json:get_value(<<"pvt_billing_day">>, AccountVarsJObj) of
        'undefined' ->
            JObj = set_billing_day(AccountId),
            kz_json:get_value(<<"pvt_billing_day">>, JObj);
        BDay -> BDay
    end.

set_billing_day(AccountId) ->
    {{_,_,Today},_} = calendar:universal_time(),
    case maybe_allow_postpay(AccountId) of
        {'true', _} ->
            case maybe_force_postpay_billing_day(AccountId) of
                'true' -> set_billing_day(1, AccountId);
                'false' -> set_billing_day(Today, AccountId)
            end;
        'false' ->
            case maybe_force_prepay_billing_day(AccountId) of
                'true' -> set_billing_day(1, AccountId);
                'false' -> set_billing_day(Today, AccountId)
            end
    end.

-spec set_billing_day(integer(), ne_binary()) -> kz_json:object().
set_billing_day(BillingDay, AccountId) ->
    DbName = kz_util:format_account_id(AccountId,'encoded'),
    case kz_datamgr:open_doc(DbName, ?ONBILL_DOC) of
        {ok, Doc} ->
            NewDoc = kz_json:set_value(<<"pvt_billing_day">>, BillingDay, Doc),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            NewDoc;
        {'error', 'not_found'} ->
            NewDoc = kz_json:set_values([{<<"_id">>, ?ONBILL_DOC}
                                        ,{<<"pvt_type">>, ?ONBILL_DOC}
                                        ,{<<"pvt_account_id">>, AccountId}
                                        ,{<<"pvt_billing_day">>, BillingDay}
                                        ]
                                       ,kz_json:new()),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            NewDoc;
        _ ->
            kz_json:set_values([{<<"_id">>, ?ONBILL_DOC}
                               ,{<<"pvt_type">>, ?ONBILL_DOC}
                               ,{<<"pvt_account_id">>, AccountId}
                               ,{<<"pvt_billing_day">>, BillingDay}
                               ]
                              ,kz_json:new())
    end.

-spec account_creation_date(ne_binary()) -> {kz_year(), kz_month(), kz_day()}.
account_creation_date(AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    Timestamp = kz_json:get_value(<<"pvt_created">>, AccountDoc),
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    {Year, Month, Day}.

-spec account_creation_ts(ne_binary()) -> integer().
account_creation_ts(AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    kz_json:get_value(<<"pvt_created">>, AccountDoc).

-spec maybe_allow_postpay(ne_binary()) -> boolean().
maybe_allow_postpay(AccountId) ->
    Limits = j5_limits:get(AccountId),
    case j5_limits:allow_postpay(Limits) of
        'false' -> 'false';
        'true' -> {'true', j5_limits:max_postpay(Limits)}
    end.

-spec trial_has_expired(ne_binary() | kz_account:doc()) -> boolean().
trial_has_expired(AccountId) when is_binary(AccountId) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    trial_has_expired(AccountJObj);
trial_has_expired(AccountJObj) ->
    kz_account:trial_has_expired(AccountJObj).

-spec is_trial_account(ne_binary() | kz_account:doc()) -> boolean().
is_trial_account(AccountId) when is_binary(AccountId) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    is_trial_account(AccountJObj);
is_trial_account(AccountJObj) ->
    case kz_account:trial_expiration(AccountJObj) of
        'undefined' -> 'false';
        _ -> 'true'
    end.

-spec maybe_convicted(ne_binary()) -> 'ok'|'delinquent'.
maybe_convicted(AccountId) ->
    case maybe_below_waterline(AccountId) of
        'true' -> 'true';
        'false' -> false;
        'even' ->
            not (account_creation_date(AccountId) == period_start_date(AccountId))
    end.

maybe_below_waterline(AccountId) ->
    Balance = current_balance(AccountId),
    MPpay = 
        case maybe_allow_postpay(AccountId) of
            'false' -> 0;
            {'true', MaxPostpay} ->  abs(MaxPostpay)
        end,
    case Balance + MPpay of
        Draft when Draft == 0 -> 'even';
        Draft when Draft < 0 -> 'true';
        Draft when Draft > 0 -> 'false'
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

-spec current_service_status(ne_binary()) -> ne_binary().
current_service_status(AccountId) ->
    {'ok', ServicesJObj} = kz_services:fetch_services_doc(AccountId,'false'),
    kzd_services:status(ServicesJObj).

-spec is_service_plan_assigned(ne_binary()) -> boolean().
is_service_plan_assigned(AccountId) ->
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    Plans = kz_service_plans:plan_summary(ServicesJObj),
    not kz_term:is_empty(Plans).

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
    _ = ensure_service_plan(AccountId),
    set_billing_day(AccountId),
    {'ok', Doc} = kz_account:fetch(AccountId),
    NewDoc = kz_json:delete_key(?KEY_TRIAL_EXPIRATION, Doc),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', JObj} = kz_datamgr:ensure_saved(AccountDb, NewDoc),
    _ = replicate_account_doc(JObj),
    reconcile_and_sync(AccountId),
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

-spec reconcile_and_sync(ne_binary()) -> any().
reconcile_and_sync(AccountId) ->
    _ = kz_services:reconcile(AccountId),
    kz_service_sync:sync(AccountId).

-spec maybe_save_as_dirty(ne_binary()) -> any().
maybe_save_as_dirty(AccountId) ->
    case kz_services:is_dirty(kz_services:fetch(AccountId)) of
        'true' -> 'ok';
        'false' ->
            kz_services:save_as_dirty(AccountId)
    end.

-spec current_balance(ne_binary()) -> number().
current_balance(AccountId) ->
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> Balance;
        _ -> 0
    end.

-spec current_account_dollars(ne_binary()) -> number().
current_account_dollars(AccountId) ->
    case wht_util:current_account_dollars(AccountId) of
        {'ok', Balance} -> Balance;
        _ -> 0
    end.

-spec maybe_process_new_billing_period(ne_binary()) -> boolean().
maybe_process_new_billing_period(AccountId) ->
    {Year, Month, _} = period_start_date(AccountId, kz_time:current_tstamp()),
    case kazoo_modb:open_doc(AccountId, ?MRC_DOC, Year, Month) of
        {'ok', _} -> 'false';
        {'error', 'not_found'} -> 'true'
    end.

-spec list_account_periods(ne_binary()) -> kz_json:objects().
list_account_periods(AccountId) ->
    {Year, Month, _Day} = account_creation_date(AccountId),
    BillingDay = kz_term:to_integer(billing_day(AccountId)),
    Timestamp = calendar:datetime_to_gregorian_seconds({{Year, Month, BillingDay}, {0,0,0}}),
    TS_Now =  kz_time:current_tstamp(),
    list_account_periods(AccountId, BillingDay, Timestamp, TS_Now, []).

list_account_periods(_, _, Timestamp, TS_Now, Acc) when Timestamp > TS_Now ->
    Acc;
list_account_periods(AccountId, BillingDay, Timestamp, TS_Now, Acc) ->
    ThisPeriod = {[{<<"period_start">>, date_json(period_start_date(AccountId, Timestamp))}
                  ,{<<"period_end">>, date_json(period_end_date(AccountId, Timestamp))}]},
    DataBag = kz_json:set_values([{[<<"period_start">>, <<"billing_day">>], BillingDay}
                                 ,{[<<"period_end">>, <<"billing_day">>], BillingDay}
                                 ]
                                ,ThisPeriod),
    NextTimestamp =
        calendar:datetime_to_gregorian_seconds({next_period_start_date(AccountId, Timestamp), {0,0,0}}),
    list_account_periods(AccountId, BillingDay, NextTimestamp, TS_Now, [DataBag] ++ Acc).

-spec period_openning_balance(ne_binary(), kz_year(), kz_month(), kz_day()) -> number() | {'error', any()}.
period_openning_balance(AccountId, Year, Month, Day) ->
    {SYear, SMonth, SDay} = period_start_date(AccountId, Year, Month, Day),
    day_start_balance(AccountId, SYear, SMonth, SDay).

-spec period_openning_balance_dollars(ne_binary(), kz_year(), kz_month(), kz_day()) -> number() | {'error', any()}.
period_openning_balance_dollars(AccountId, Year, Month, Day) ->
    case period_openning_balance(AccountId, Year, Month, Day) of
        Balance when is_number(Balance) ->
            wht_util:units_to_dollars(Balance);
        Balance ->
            Balance
    end.

-spec day_start_balance(ne_binary(), kz_year(), kz_month(), kz_day()) -> number() | {'error', any()}.
day_start_balance(AccountId, Year, Month, 1) ->
    case kazoo_modb:open_doc(AccountId, <<"monthly_rollup">>, Year, Month) of
        {'ok', JObj} -> get_amount(JObj);
        {'error', 'not_found'} -> 
            {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            case wht_util:previous_balance(AccountId, PrevYear, PrevMonth) of
                {'ok', Balance} ->
                    Balance;
                {'error', _E} = Error ->
                    lager:warning("unable to get period_start_balance for ~s: ~p", [AccountId, _E]),
                    Error
            end
    end;
day_start_balance(AccountId, Year, Month, Day) ->
    View = <<"onbills/debit_credit_timestamp">>,
    Timestamp = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0,0,0}}),
    maybe_add_design_doc(kazoo_modb:get_modb(AccountId, Year, Month), <<"onbills">>),
    ViewOptions = [{'year', kz_term:to_binary(Year)}
                  ,{'month', kz_time:pad_month(Month)}
                  ,{'endkey', Timestamp}
                  ],
    case kazoo_modb:get_results(AccountId, View, ViewOptions) of
        {'ok', []} ->
            day_start_balance(AccountId, Year, Month, 1);
        {'ok', [ViewRes|_]} ->
            kz_json:get_integer_value(<<"value">>, ViewRes, 0)
            + day_start_balance(AccountId, Year, Month, 1);
        {'error', _E} = Error ->
            lager:warning("unable to get period_start_balance for ~s (~p-~p-~p): ~p", [AccountId, Year, Month, Day, _E]),
            Error
    end.

-spec day_start_balance_dollars(ne_binary(), kz_year(), kz_month(), kz_day()) -> number() | {'error', any()}.
day_start_balance_dollars(AccountId, Year, Month, Day) ->
    case day_start_balance(AccountId, Year, Month, Day) of
        Balance when is_number(Balance) ->
            wht_util:units_to_dollars(Balance);
        Balance ->
            Balance
    end.

-spec get_amount(kz_json:object()) -> number().
get_amount(JObj) ->
    Amount = kz_json:get_first_defined([<<"pvt_amount">>,<<"amount">>], JObj, 0),
    Type = kz_json:get_first_defined([<<"pvt_type">>,<<"type">>], JObj),
    case Type of
        <<"debit">> -> Amount*-1;
        _ -> Amount
    end.

-spec get_range(ne_binary(), gregorian_seconds(), gregorian_seconds()) -> kz_proplists().
get_range(AccountId, From, To) ->
    [ begin
          {AccountId, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
          [{'startkey', From}
          ,{'endkey', To}
          ,{'year', Year}
          ,{'month', Month}
          ]
      end || MODb <- kazoo_modb:get_range(AccountId, From, To)
    ].


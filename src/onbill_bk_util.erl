-module(onbill_bk_util).

-export([max_daily_usage_exceeded/3
        ,prepare_dailyfee_doc_name/3
        ,select_daily_count_items_list/2
        ,select_daily_count_items_json/2
        ,select_non_zero_items_json/1
        ,select_non_zero_items_list/2
        ,save_dailyfee_doc/5
        ,charge_newly_added/4
        ,process_new_billing_period_mrc/2
        ,maybe_cancel_trunk_subscriptions/1
        ,items_amount/3
        ,calc_item/2
        ,current_usage_amount/1
        ,current_usage_amount_in_units/1
        ,today_dailyfee_absent/1
        ,maybe_issue_previous_billing_period_docs/4
        ]).

-include("onbill.hrl").
-include_lib("/opt/kazoo/core/braintree/include/braintree.hrl").
-include_lib("/opt/kazoo/core/kazoo/include/kz_databases.hrl").
-include_lib("/opt/kazoo/core/kazoo_transactions/include/kazoo_transactions.hrl").

-spec max_daily_usage_exceeded(kz_service_item:items(), ne_binary(), integer()) -> any().
max_daily_usage_exceeded(Items, AccountId, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    DailyFeeId = prepare_dailyfee_doc_name(Year, Month, Day),
    case kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month) of
        {'ok', DFDoc} ->
            case kz_json:get_value([<<"pvt_metadata">>,<<"max_usage">>,<<"all_items">>], DFDoc) of
                'undefined' ->
                    {'true', select_non_zero_items_json(Items), []};
                 MaxJObj ->
                    NewJObj = select_non_zero_items_json(Items),
                    case check_max_usage(NewJObj, MaxJObj, Timestamp) of
                        [] ->
                            'false';
                        Diff ->
                            NewMaxJObj = kz_json:set_values(Diff, MaxJObj),
                            _ = update_dailyfee_max(Timestamp, AccountId, NewMaxJObj, Items),
                            {'true', NewMaxJObj, excess_details(NewJObj, MaxJObj)}
                    end
            end;
        {'error', 'not_found'} ->
            ItemsJObj = select_non_zero_items_json(Items),
            _ = create_dailyfee_doc(Timestamp, AccountId, 0, ItemsJObj, Items),
            {'true', ItemsJObj, []}
    end.

-spec prepare_dailyfee_doc_name(integer(), integer(), integer()) -> ne_binary().
prepare_dailyfee_doc_name(Y, M, D) ->
    Year = kz_term:to_binary(Y),
    Month = kz_time:pad_month(M),
    Day = kz_time:pad_month(D),
    <<Year/binary, Month/binary, Day/binary, "-dailyfee">>.

-spec select_daily_count_items_list(kz_service_item:items(), ne_binary()) -> proplist().
select_daily_count_items_list(Items, AccountId) ->
    case kz_json:is_json_object(Items) of
        'false' ->
            select_daily_count_items_list(select_non_zero_items_json(Items), AccountId);
        'true' ->
            ResellerVars = onbill_util:reseller_vars(AccountId),
            DailyItemsPaths = lists:foldl(fun (Category, Acc) -> [[Category, Item] || Item <- kz_json:get_keys(Category, Items)] ++ Acc end
                                       ,[]
                                       ,kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, [])
                                       ),
            [kz_json:get_value(ItemPath, Items) || ItemPath <- DailyItemsPaths]
    end.

-spec select_daily_count_items_json(kz_service_item:items()|kz_json:object(), ne_binary()) -> kz_json:object().
select_daily_count_items_json(Items, AccountId) ->
    case kz_json:is_json_object(Items) of
        'false' ->
            select_daily_count_items_json(select_non_zero_items_json(Items), AccountId);
        'true' ->
            ResellerVars = onbill_util:reseller_vars(AccountId),
            CategoriesList = kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, []),
            Upd = [{Category, kz_json:get_value(Category, Items)}
                   || Category <- CategoriesList
                  ],
            kz_json:set_values(Upd, kz_json:new())
    end.

-spec select_non_zero_items_json(kz_service_item:items()) -> kz_json:object().
select_non_zero_items_json(Items) ->
    Upd = [{[kz_service_item:category(Item), kz_service_item:item(Item)], kz_service_item:public_json(Item)}
           || Item <- kz_service_items:to_list(Items)
           ,kz_service_item:quantity(Item) > 0
    ],
    kz_json:set_values(Upd, kz_json:new()).

-spec select_non_zero_items_list(kz_service_item:items()|kz_json:object(), ne_binary()) -> proplist().
select_non_zero_items_list(Items, AccountId) ->
    case kz_json:is_json_object(Items) of
        'false' ->
            select_non_zero_items_list(select_non_zero_items_json(Items), AccountId);
        'true' ->
            ItemsPaths = lists:foldl(fun (Category, Acc) -> [[Category, Item] || Item <- kz_json:get_keys(Category, Items)] ++ Acc end
                                    ,[]
                                    ,kz_json:get_keys(Items)
                                    ),
            [kz_json:get_value(ItemPath, Items) || ItemPath <- ItemsPaths]
    end.

check_max_usage(NewJObj, MaxJObj, Timestamp) ->
    ItemsPathList = lists:foldl(fun (Category, Acc) -> [[Category, Item] || Item <- kz_json:get_keys(Category, NewJObj)] ++ Acc end
                               ,[]
                               ,kz_json:get_keys(NewJObj)
                               ),
    [{ItemPath, kz_json:set_value(<<"updated">>, Timestamp, kz_json:get_value(ItemPath, NewJObj))}
     || ItemPath <- ItemsPathList
     ,kz_json:get_value(ItemPath ++ [<<"quantity">>], NewJObj, 0) > kz_json:get_value(ItemPath ++ [<<"quantity">>], MaxJObj, 0)
    ].

excess_details(NewJObj, MaxJObj) ->
    ItemsPathList = lists:foldl(fun (Category, Acc) -> [[Category, Item] || Item <- kz_json:get_keys(Category, NewJObj)] ++ Acc end
                               ,[]
                               ,kz_json:get_keys(NewJObj)
                               ),
    [{ItemPath, kz_json:get_value(ItemPath ++ [<<"quantity">>], NewJObj, 0) - kz_json:get_value(ItemPath ++ [<<"quantity">>], MaxJObj, 0)}
     || ItemPath <- ItemsPathList
     ,kz_json:get_value(ItemPath ++ [<<"quantity">>], NewJObj, 0) > kz_json:get_value(ItemPath ++ [<<"quantity">>], MaxJObj, 0)
    ].



-spec update_dailyfee_max(integer(), ne_binary(), any(), any()) -> any().
update_dailyfee_max(Timestamp, AccountId, MaxUsage, Items) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    DailyFeeId = prepare_dailyfee_doc_name(Year, Month, Day),
    case kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month) of
        {'error', 'not_found'} ->
            create_dailyfee_doc(Timestamp, AccountId, 0, MaxUsage, Items);
        {'ok', DFDoc} ->
            Upd = [{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], select_non_zero_items_json(Items)}
                  ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"all_items">>], MaxUsage}
                  ],
            NewDoc = kz_json:set_values(Upd, DFDoc),
            kazoo_modb:save_doc(AccountId, NewDoc, Year, Month)
    end.

-spec save_dailyfee_doc(integer(), ne_binary(), number(), any(), any()) -> any().
save_dailyfee_doc(Timestamp, AccountId, Amount, MaxUsage, Items) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    DailyFeeId = prepare_dailyfee_doc_name(Year, Month, Day),
    case kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month) of
        {'error', 'not_found'} -> create_dailyfee_doc(Timestamp, AccountId, Amount, MaxUsage, Items);
        {'ok', DFDoc} -> update_dailyfee_doc(Timestamp, AccountId, Amount, MaxUsage, Items, DFDoc)
    end.

-spec create_dailyfee_doc(integer(), ne_binary(), number(), any(), any()) -> any().
create_dailyfee_doc(Timestamp, AccountId, Amount, MaxUsage, Items) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    MonthStrBin = kz_term:to_binary(httpd_util:month(Month)),
    Routines = [{<<"_id">>, prepare_dailyfee_doc_name(Year, Month, Day)}
               ,{<<"pvt_type">>, <<"debit">>}
               ,{<<"description">>,<<(?TO_BIN(Day))/binary," ",MonthStrBin/binary," ",(?TO_BIN(Year))/binary," daily fee">>}
               ,{<<"pvt_reason">>, <<"daily_fee">>}
               ,{[<<"pvt_metadata">>,<<"days_in_period">>], onbill_util:days_in_period(AccountId, Year, Month, Day)}
               ,{<<"pvt_created">>, Timestamp}
               ,{<<"pvt_modified">>, Timestamp}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_account_db">>, kazoo_modb:get_modb(AccountId, Year, Month)}
              ],
    BrandNewDoc = kz_json:set_values(Routines, kz_json:new()),
    Upd = dailyfee_doc_update_routines(Timestamp, AccountId, Amount, MaxUsage, Items),
    kazoo_modb:save_doc(AccountId, kz_json:set_values(Upd, BrandNewDoc), Year, Month).

-spec update_dailyfee_doc(integer(), ne_binary(), number(), any(), any(), kz_json:object()) -> any().
update_dailyfee_doc(Timestamp, AccountId, Amount, MaxUsage, Items, DFDoc) ->
    Upd = dailyfee_doc_update_routines(Timestamp, AccountId, Amount, MaxUsage, Items),
    NewDoc = kz_json:set_values(Upd, DFDoc),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    kazoo_modb:save_doc(AccountId, NewDoc, Year, Month).

-spec dailyfee_doc_update_routines(gregorian_seconds(), ne_binary(), number(), kz_json:object(), kz_service_item:items()) -> any().
dailyfee_doc_update_routines(Timestamp, AccountId, Amount, MaxUsage, Items) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    [{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], select_non_zero_items_json(Items)}
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"all_items">>], MaxUsage}
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"daily_calculated_items">>] ,select_daily_count_items_json(MaxUsage, AccountId) }
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"monthly_amount_of_daily_calculated_items">>], wht_util:dollars_to_units(Amount)}
     ,{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month)}
     ,{<<"pvt_modified">>, Timestamp}
    ].

-spec charge_newly_added(ne_binary(), kz_json:object(), proplist(), integer()) -> 'ok'|proplist(). 
charge_newly_added(_AccountId, _NewMax, [], _Timestamp) -> 'ok';
charge_newly_added(AccountId, NewMax, [{[Category,_] = Path, Qty}|ExcessDets], Timestamp) -> 
    ResellerVars = onbill_util:reseller_vars(AccountId),
    DailyCountCategoriesList = kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, []),
    case lists:member(Category, DailyCountCategoriesList) of
        'true' -> 'ok';
        'false' ->
            DaysInPeriod = onbill_util:days_in_period(AccountId, Timestamp),
            DaysLeft = onbill_util:days_left_in_period(AccountId, Timestamp),
            ItemJObj = kz_json:get_value(Path, NewMax),
            Ratio = DaysLeft / DaysInPeriod,
	    Reason =
	        case Ratio of
		    1.0 -> <<"monthly_recurring">>;
		    _ -> <<"recurring_prorate">>
		end,
            Upd =
                case kz_json:get_value(<<"apply_discount_to_prorated">>, ResellerVars, 'false')
                     orelse Ratio == 1.0
                of
                    'true' ->
                        %% TODO if anyone needed:
                        %% Apply single discount if it is the very first unit
                        %% Count if cumulative discounts left
                        discount_newly_added(Qty, ItemJObj);
                    _ ->
                        [{<<"quantity">>, Qty}
                        ,{<<"single_discount">>, false}
                        ,{<<"cumulative_discount">>, 0.0}
                        ,{<<"cumulative_discount_rate">>, 0.0}
                        ]
                end,
            create_debit_tansaction(AccountId, kz_json:set_values(Upd, ItemJObj), Timestamp, Reason, Ratio)
    end,
    charge_newly_added(AccountId, NewMax, ExcessDets, Timestamp). 

discount_newly_added(Qty, ItemJObj) ->
    SingleDiscount =
        (kz_json:get_value(<<"quantity">>, ItemJObj) == Qty)
        andalso (kz_json:get_value(<<"single_discount">>, ItemJObj) == 'true'),
    ItemJObjQuantity = kz_json:get_value(<<"quantity">>, ItemJObj),
    CumulativeDiscount =
        case kz_json:get_value(<<"cumulative_discount">>, ItemJObj) of
            ItemJObjQuantity ->
                Qty;
            JObjCumulativeDiscount when (ItemJObjQuantity - JObjCumulativeDiscount) > Qty ->
                0.0;
            JObjCumulativeDiscount ->
                ItemJObjQuantity - JObjCumulativeDiscount
        end,
    [{<<"quantity">>, Qty}
    ,{<<"single_discount">>, SingleDiscount}
    ,{<<"cumulative_discount">>, CumulativeDiscount}
    ].

-spec process_new_billing_period_mrc(ne_binary(), gregorian_seconds()) -> 'ok'|proplist(). 
process_new_billing_period_mrc(AccountId, Timestamp) ->
    case onbill_bk_util:current_usage_amount_in_units(AccountId)
        > (onbill_util:current_balance(AccountId) + abs(j5_limits:max_postpay(j5_limits:get(AccountId))))
    of
        'true' ->
            lager:debug("not sufficient amount of funds for charging monthly_recurring services"),
            maybe_cancel_trunk_subscriptions(AccountId);
        'false' ->
            lager:debug("amount of funds enough for charging monthly_recurring services"),
            Items = current_items(AccountId),
            ItemsJObj = select_non_zero_items_json(Items),
            charge_new_billing_period_mrc(ItemsJObj, AccountId, Timestamp),
            DataBag = onbill_notifications:customer_update_databag(AccountId),
            onbill_notifications:send_account_update(AccountId, ?MRC_TEMPLATE, DataBag),
            {'ok', 'mrc_processed'}
    end.

-spec maybe_cancel_trunk_subscriptions(ne_binary()) -> {'not_enough_funds', 'no_trunks_set'}
                                                       |{'not_enough_funds', 'trunks_canceled'}. 
maybe_cancel_trunk_subscriptions(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, <<"limits">>) of
        {'error', _R} ->
            lager:debug("unable (~p) to get current limits for: ~p, assuming no limits set", [_R, AccountId]),
            {'not_enough_funds', 'no_trunks_set'};
        {'ok', LimitsDoc} ->
            case kz_json:get_integer_value(<<"twoway_trunks">>, LimitsDoc, 0) =/= 0
                orelse kz_json:get_integer_value(<<"inbound_trunks">>, LimitsDoc, 0) =/= 0
                orelse kz_json:get_integer_value(<<"outbound_trunks">>, LimitsDoc, 0) =/= 0
            of
                'true' ->
                    Keys = [<<"twoway_trunks">>, <<"inbound_trunks">>, <<"outbound_trunks">>],
                    NewLimitsDoc = kz_json:delete_keys(Keys, LimitsDoc),
                    kz_datamgr:ensure_saved(AccountDb, NewLimitsDoc),
                    kzs_cache:flush_cache_doc(AccountDb, NewLimitsDoc),
                    j5_limits:fetch(AccountId),
                    kz_services:reconcile(AccountId),
                    DataBag = kz_json:set_value(<<"limits">>
                                               ,LimitsDoc
                                               ,onbill_notifications:customer_update_databag(AccountId)),
                    onbill_notifications:send_account_update(AccountId, ?LIMITS_SET_TO_ZERO_TEMPLATE, DataBag),
                    {'not_enough_funds', 'trunks_canceled'};
                'false' ->
                    lager:debug("Zero limits already set for ~p", [AccountId]),
                    {'not_enough_funds', 'no_trunks_set'}
            end
    end.

charge_new_billing_period_mrc(ItemsJObj, AccountId, Timestamp) ->
    {'ok',_} = create_monthly_recurring_doc(AccountId, ItemsJObj, Timestamp),
    [charge_mrc_category(AccountId, Category, ItemsJObj, Timestamp)
     || Category <- kz_json:get_keys(ItemsJObj)
     ,not lists:member(Category, kz_json:get_value(<<"pvt_daily_count_categories">>, onbill_util:reseller_vars(AccountId), []))
    ].

-spec create_monthly_recurring_doc(ne_binary(), kz_json:object(), gregorian_seconds()) -> any().
create_monthly_recurring_doc(AccountId, NewMax, Timestamp) ->
    {Year, Month, Day} = onbill_util:period_start_date(AccountId, Timestamp),
    MonthStrBin = kz_term:to_binary(httpd_util:month(Month)),
    Routines = [{<<"_id">>, <<"monthly_recurring">>}
               ,{<<"description">>,<<"MRC info for period start: ",(?TO_BIN(Day))/binary," ",MonthStrBin/binary," ",(?TO_BIN(Year))/binary>>}
               ,{[<<"pvt_metadata">>,<<"items">>], NewMax}
               ,{<<"pvt_period_openning_balance">>, onbill_util:day_start_balance(AccountId, Year, Month, Day)}
               ,{<<"pvt_created">>, Timestamp}
               ,{<<"pvt_modified">>, Timestamp}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_account_db">>, kazoo_modb:get_modb(AccountId, Year, Month)}
              ],
    BrandNewDoc = kz_json:set_values(Routines, kz_json:new()),
    kazoo_modb:save_doc(AccountId, BrandNewDoc, Year, Month).

-spec charge_mrc_category(ne_binary(), ne_binary(), kz_json:object(), gregorian_seconds()) -> any().
charge_mrc_category(AccountId, Category, NewMax, Timestamp) ->
    ItemsList = kz_json:get_keys(kz_json:get_value(Category, NewMax)),
    [charge_mrc_item(AccountId, kz_json:get_value([Category,Item],NewMax), Timestamp)
     || Item <- ItemsList
    ].

-spec charge_mrc_item(ne_binary(), kz_json:object(), gregorian_seconds()) -> 'ok'|proplist(). 
charge_mrc_item(AccountId, ItemJObj, Timestamp) ->
    create_debit_tansaction(AccountId, ItemJObj, Timestamp, <<"monthly_recurring">>, 1.0).

-spec create_debit_tansaction(ne_binary(), kz_json:object(), gregorian_seconds(), ne_binary(), float()) -> 'ok'|proplist(). 
create_debit_tansaction(AccountId, ItemJObj, Timestamp, Reason, Ratio) ->
    CItem = calc_item(ItemJObj, AccountId),
    DiscountedItemCost = kz_json:get_float_value(<<"discounted_item_cost">>, CItem),
    UnitsAmount = wht_util:dollars_to_units(DiscountedItemCost) * Ratio,
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    MonthStr = httpd_util:month(Month),

    Upd = [{<<"account_id">>, AccountId}
          ,{<<"date">>, <<(?TO_BIN(Day))/binary," ",(?TO_BIN(MonthStr))/binary," ",(?TO_BIN(Year))/binary>>}
          ,{<<"timestamp">>, Timestamp}
          ,{<<"reason">>, Reason}
          ,{<<"ratio">>, Ratio}
          ,{<<"amount">>, UnitsAmount}
          ,{<<"item_jobj">>, ItemJObj}
          ],

    Routines = [fun(Tr) -> kz_transaction:set_reason(Reason, Tr) end
               ,fun(Tr) -> kz_transaction:set_description(kz_json:get_value(<<"name">>, CItem), Tr) end
               ,fun(Tr) -> kz_transaction:set_metadata(kz_json:set_values(Upd, CItem), Tr) end
               ,fun kz_transaction:save/1
               ],

    lists:foldl(
      fun(F, Tr) -> F(Tr) end
               ,kz_transaction:debit(AccountId, UnitsAmount)
               ,Routines
     ).

-spec items_amount(kz_json:objects(), ne_binary(), float()) -> number().
items_amount([], _, Acc) ->
    Acc;
items_amount([ServiceItem|ServiceItems], AccountId, Acc) ->
    ItemCost = item_cost(ServiceItem, AccountId),
    SubTotal = Acc + ItemCost,
    items_amount(ServiceItems, AccountId, SubTotal).

-spec item_cost(kz_json:object(), ne_binary()) -> number().
item_cost(ItemJObj, AccountId) ->
    kz_json:get_value(<<"discounted_item_cost">>, calc_item(ItemJObj, AccountId)).

-spec calc_item(kz_json:object(), ne_binary()) -> number().
calc_item(ItemJObj, AccountId) ->
    try
        ResellerVars = onbill_util:reseller_vars(AccountId),
        CurrencySign = kz_json:get_value(<<"currency_sign">>, ResellerVars, <<"£"/utf8>>),
        Quantity = kz_json:get_value(<<"quantity">>, ItemJObj),
        Rate = kz_json:get_value(<<"rate">>, ItemJObj),
        SingleDiscountAmount =
            case kz_json:get_value(<<"single_discount">>, ItemJObj) of
                'true' -> kz_json:get_value(<<"single_discount_rate">>, ItemJObj);
                _ -> 0
            end,
        CumulativeDiscount = kz_json:get_value(<<"cumulative_discount">>, ItemJObj),
        CumulativeDiscountRate = kz_json:get_value(<<"cumulative_discount_rate">>, ItemJObj),
        TotalDiscount = SingleDiscountAmount + CumulativeDiscount * CumulativeDiscountRate,
        ItemCost = Rate * Quantity,
        DiscountedItemCost = ItemCost - TotalDiscount,
        {[{<<"name">>, kz_json:get_value(<<"name">>, ItemJObj)}
        ,{<<"quantity">>, Quantity}
        ,{<<"rate">>, Rate}
        ,{<<"single_discount_amount">>, SingleDiscountAmount}
        ,{<<"cumulative_discount">>, CumulativeDiscount}
        ,{<<"cumulative_discount_rate">>, CumulativeDiscountRate}
        ,{<<"total_discount">>, TotalDiscount}
        ,{<<"discounted_item_cost">>, DiscountedItemCost}
        ,{<<"item_cost">>, ItemCost}
        ,{<<"pp_rate">>, currency_sign:add_currency_sign(CurrencySign , Rate)}
        ,{<<"pp_total_discount">>, currency_sign:add_currency_sign(CurrencySign , TotalDiscount)}
        ,{<<"pp_item_cost">>, currency_sign:add_currency_sign(CurrencySign , ItemCost)}
        ,{<<"pp_discounted_item_cost">>, currency_sign:add_currency_sign(CurrencySign , DiscountedItemCost)}
        ]}
    catch
        E:R ->
            lager:debug("exception syncing acount: ~p : ~p: ~p", [AccountId, E, R]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, ItemJObj]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, kz_json:get_value(<<"rate">>, ItemJObj)]),
            Subj = io_lib:format("OnBill Bookkeeper syncing problem! AccountId: ~p",[AccountId]),
            Msg = io_lib:format("Exception syncing AccountId: ~p <br /> Service Item: ~p <br /> Rate: ~p"
                               ,[AccountId
                                ,ItemJObj
                                ,kz_json:get_value(<<"rate">>, ItemJObj)
                                ]),
            kz_notify:system_alert(Subj, Msg, []),
            {[{<<"name">>, kz_json:get_value(<<"name">>, ItemJObj)}
            ,{<<"quantity">>, 0}
            ,{<<"rate">>, 0.0}
            ,{<<"single_discount_amount">>, 0.0}
            ,{<<"cumulative_discount">>, 0.0}
            ,{<<"cumulative_discount_rate">>, 0.0}
            ,{<<"total_discount">>, 0.0}
            ,{<<"discounted_item_cost">>, 0.0}
            ,{<<"item_cost">>, 0.0}
            ]}
    end.

-spec current_items(ne_binary()) -> kz_service_item:items().
current_items(AccountId) -> 
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    {'ok', Items} = kz_service_plans:create_items(ServicesJObj),
    Items.

-spec current_usage_amount_in_units(ne_binary()) -> float().
current_usage_amount_in_units(AccountId) ->
    wht_util:dollars_to_units(current_usage_amount(AccountId)).

-spec current_usage_amount(ne_binary()) -> float().
current_usage_amount(AccountId) ->
    Items = current_items(AccountId),
    ItemsJObj = select_non_zero_items_list(Items,AccountId),
    items_amount(ItemsJObj, AccountId, 0.0).

-spec today_dailyfee_absent(ne_binary()) -> boolean().
today_dailyfee_absent(AccountId) ->
    Timestamp = kz_time:current_tstamp(),
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    DailyFeeId = prepare_dailyfee_doc_name(Year, Month, Day),
    case kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month) of
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec maybe_issue_previous_billing_period_docs(ne_binary(), kz_year(), kz_month(), kz_day()) -> any().
maybe_issue_previous_billing_period_docs(AccountId, Year, Month, Day) ->
    {PYear, PMonth, PDay} = onbill_util:period_start_date(AccountId, Year, Month, Day),
    _ = kz_util:spawn(fun onbill_docs:generate_docs/4, [AccountId, PYear, PMonth, PDay]).

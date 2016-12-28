-module(onbill_bk_util).

-export([max_daily_usage_exceeded/3
        ,prepare_dailyfee_doc_name/3
        ,select_daily_count_items_list/2
        ,select_daily_count_items_json/2
        ,select_non_zero_items_json/1
        ,save_dailyfee_doc/5
        ,charge_newly_added/4
        ,check_this_period_mrc/3
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
            _ = create_dailyfee_doc(Timestamp, AccountId, 0, select_non_zero_items_json(Items), Items),
            {'true', select_non_zero_items_json(Items), []}
    end.

-spec prepare_dailyfee_doc_name(integer(), integer(), integer()) -> ne_binary().
prepare_dailyfee_doc_name(Y, M, D) ->
    Year = kz_util:to_binary(Y),
    Month = kz_util:pad_month(M),
    Day = kz_util:pad_month(D),
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

-spec select_daily_count_items_json(kz_service_item:items(), ne_binary()) -> kz_json:object().
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
    MonthStrBin = kz_util:to_binary(httpd_util:month(Month)),
    Routines = [{<<"_id">>, prepare_dailyfee_doc_name(Year, Month, Day)}
               ,{<<"pvt_type">>, <<"debit">>}
               ,{<<"description">>,<<(?TO_BIN(Day))/binary," ",MonthStrBin/binary," ",(?TO_BIN(Year))/binary," daily fee">>}
               ,{<<"pvt_reason">>, <<"daily_fee">>}
               ,{[<<"pvt_metadata">>,<<"days_in_period">>], calendar:last_day_of_the_month(Year, Month)}
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
    DailyCountCategoriesList = kz_json:get_value(<<"pvt_daily_count_categories">>, onbill_util:reseller_vars(AccountId), []),
    case lists:member(Category, DailyCountCategoriesList) of
        'true' -> 'ok';
        'false' ->
            {StartYear, StartMonth, StartDay} = onbill_util:period_start_date(AccountId, Timestamp),
            DaysInPeriod = onbill_util:days_in_period(StartYear, StartMonth, StartDay),
            DaysLeft = onbill_util:days_left_in_period(StartYear, StartMonth, StartDay, Timestamp),
            ItemJObj = kz_json:get_value(Path, NewMax),
            Rate = kz_json:get_float_value(<<"rate">>, ItemJObj),
            ProratedRate = Rate * DaysLeft / DaysInPeriod,
            Units = wht_util:dollars_to_units(ProratedRate),
            Amount = Units * Qty,
            create_debit_tansaction(AccountId, ItemJObj, Timestamp, <<"recurring_prorate">>, Qty, Amount, ProratedRate)
    end,
    charge_newly_added(AccountId, NewMax, ExcessDets, Timestamp). 

-spec check_this_period_mrc(ne_binary(), kz_json:object(), gregorian_seconds()) -> 'ok'|proplist(). 
check_this_period_mrc(AccountId, NewMax, Timestamp) ->
    {Year, Month, Day} = onbill_util:period_start_date(AccountId, Timestamp),
    case kazoo_modb:open_doc(AccountId, ?MRC_DOC, Year, Month) of
        {'ok', _} ->
            'ok';
        {'error', 'not_found'} ->
            case onbill_util:get_account_created_date(AccountId) of
                {Year, Month, Day} ->
                    lager:debug("first period customer, no monthly_recurring needed");
                _ ->
                    lager:debug("monthly_recurring doc not found, trying to create"),
                    {'ok',_} = create_monthly_recurring_doc(AccountId, NewMax, Timestamp),
                    [charge_mrc_category(AccountId, Category, NewMax, Timestamp)
                     || Category <- kz_json:get_keys(NewMax)
                     ,not lists:member(Category, kz_json:get_value(<<"pvt_daily_count_categories">>, onbill_util:reseller_vars(AccountId), []))
                    ]
            end            
    end.

-spec create_monthly_recurring_doc(ne_binary(), kz_json:object(), gregorian_seconds()) -> any().
create_monthly_recurring_doc(AccountId, NewMax, Timestamp) ->
    {Year, Month, Day} = onbill_util:period_start_date(AccountId, Timestamp),
    MonthStrBin = kz_util:to_binary(httpd_util:month(Month)),
    Routines = [{<<"_id">>, <<"monthly_recurring">>}
               ,{<<"description">>,<<"MRC info for period start: ",(?TO_BIN(Day))/binary," ",MonthStrBin/binary," ",(?TO_BIN(Year))/binary>>}
               ,{[<<"pvt_metadata">>,<<"items">>], NewMax}
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
    Qty = kz_json:get_value(<<"quantity">>, ItemJObj),
    Rate = kz_json:get_float_value(<<"rate">>, ItemJObj),
    Units = wht_util:dollars_to_units(Rate),
    Amount = Units * Qty,
    create_debit_tansaction(AccountId, ItemJObj, Timestamp, <<"monthly_recurring">>, Qty, Amount, Rate).

-spec create_debit_tansaction(ne_binary(), kz_json:object(), gregorian_seconds(), ne_binary(), pos_integer(), number(), number()) -> 'ok'|proplist(). 
create_debit_tansaction(AccountId, ItemJObj, Timestamp, Reason, Qty, Amount, Rate) ->
    Name = kz_json:get_value(<<"name">>, ItemJObj),
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    MonthStr = httpd_util:month(Month),

    Description = <<(?TO_BIN(Name))/binary>>,

    Meta =
        kz_json:from_list(
          [{<<"account_id">>, AccountId}
          ,{<<"date">>, <<(?TO_BIN(Day))/binary," ",(?TO_BIN(MonthStr))/binary," ",(?TO_BIN(Year))/binary>>}
          ,{<<"reason">>, Reason}
          ,{<<"amount">>, Amount}
          ,{<<"rate">>, Rate}
          ,{<<"quantity">>, Qty}
          ,{<<"item_jobj">>, ItemJObj}
          ]
         ),

    Routines = [fun(Tr) -> kz_transaction:set_reason(Reason, Tr) end
               ,fun(Tr) -> kz_transaction:set_description(Description, Tr) end
               ,fun(Tr) -> kz_transaction:set_metadata(Meta, Tr) end
               ,fun kz_transaction:save/1
               ],
    lists:foldl(
      fun(F, Tr) -> F(Tr) end
               ,kz_transaction:debit(AccountId, Amount)
               ,Routines
     ).

-module(onbill_bk_util).

-export([max_daily_usage_exceeded/3
        ,prepare_dailyfee_doc_name/3
        ,select_daily_count_items_list/2
        ,select_daily_count_items_json/2
        ,select_non_zero_items_json/1
        ,save_dailyfee_doc/5
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
                            {'true', kz_json:set_values(Diff, MaxJObj), excess_details(NewJObj, MaxJObj)}
                    end
            end;
        {'error', 'not_found'} ->
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

dailyfee_doc_update_routines(Timestamp, AccountId, Amount, MaxUsage, Items) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    [{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], select_non_zero_items_json(Items)}
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"all_items">>], MaxUsage}
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"daily_calculated_items">>] ,select_daily_count_items_json(MaxUsage, AccountId) }
     ,{[<<"pvt_metadata">>,<<"max_usage">>,<<"monthly_amount_of_daily_calculated_items">>], wht_util:dollars_to_units(Amount)}
     ,{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month)}
     ,{<<"pvt_modified">>, Timestamp}
    ].

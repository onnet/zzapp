-module(onbill_bk_util).

-export([max_daily_usage_exceeded/3
        ,prepare_dailyfee_doc_name/3
        ,filter_out_daily_count_items_list/2
        ,filter_out_daily_count_items_json/2
        ,filter_out_items_json/1
        ,create_dailyfee_doc/4
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
                    {'true', filter_out_items_json(Items)};
                 MaxJObj ->
                    case check_max_usage(filter_out_items_json(Items), MaxJObj, Timestamp) of
                        [] ->
                            'false';
                        Diff ->
                            {'true', kz_json:set_values(Diff, MaxJObj)}
                    end
            end;
        {'error', 'not_found'} ->
            {'true', filter_out_items_json(Items)}
    end.

-spec prepare_dailyfee_doc_name(integer(), integer(), integer()) -> ne_binary().
prepare_dailyfee_doc_name(Y, M, D) ->
    Year = kz_util:to_binary(Y),
    Month = kz_util:pad_month(M),
    Day = kz_util:pad_month(D),
    <<Year/binary, Month/binary, Day/binary, "-dailyfee">>.

-spec filter_out_daily_count_items_list(kz_service_item:items(), ne_binary()) -> proplist().
filter_out_daily_count_items_list(Items, AccountId) ->
    case kz_json:is_json_object(Items) of
        'false' ->
            filter_out_daily_count_items_list(filter_out_items_json(Items), AccountId);
        'true' ->
            ResellerVars = onbill_util:reseller_vars(AccountId),
            ItemsPathList = lists:foldl(fun (Category, Acc) -> [[Category, Item] || Item <- kz_json:get_keys(Category, Items)] ++ Acc end
                                       ,[]
                                       ,kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, [])
                                       ),
            [kz_json:get_value(ItemPath, Items) || ItemPath <- ItemsPathList]
    end.

-spec filter_out_daily_count_items_json(kz_service_item:items(), ne_binary()) -> kz_json:object().
filter_out_daily_count_items_json(Items, AccountId) ->
    case kz_json:is_json_object(Items) of
        'false' ->
            filter_out_daily_count_items_json(filter_out_items_json(Items), AccountId);
        'true' ->
            ResellerVars = onbill_util:reseller_vars(AccountId),
            CategoriesList = kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, []),
            Upd = [{Category, kz_json:get_value(Category, Items)}
                   || Category <- CategoriesList
                  ],
            kz_json:set_values(Upd, kz_json:new())
    end.

-spec filter_out_items_json(kz_service_item:items()) -> kz_json:object().
filter_out_items_json(Items) ->
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

-spec create_dailyfee_doc(any(), ne_binary(), number(), integer()) -> any().
create_dailyfee_doc(Items, AccountId, Amount, Timestamp) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    MonthStrBin = kz_util:to_binary(httpd_util:month(Month)),
    Upd = [{<<"_id">>, onbill_bk_util:prepare_dailyfee_doc_name(Year, Month, Day)}
           ,{<<"pvt_type">>, <<"debit">>}
           ,{<<"description">>, <<(?TO_BIN(Day))/binary
                                  ," "
                                  ,MonthStrBin/binary
                                  ," "
                                  ,(?TO_BIN(Year))/binary
                                  ," daily fee"
                                >>
            }
           ,{<<"pvt_reason">>, <<"daily_fee">>}
           ,{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month)}
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], onbill_bk_util:filter_out_items_json(Items)}
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"daily_calculated_items">>]
            ,onbill_bk_util:filter_out_daily_count_items_json(Items, AccountId)
            }
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"monthly_amount_of_daily_calculated_items">>]
            ,wht_util:dollars_to_units(Amount)
            }
           ,{[<<"pvt_metadata">>,<<"days_in_period">>], calendar:last_day_of_the_month(Year, Month)}
           ,{<<"pvt_created">>, Timestamp}
           ,{<<"pvt_modified">>, Timestamp}
           ,{<<"pvt_account_id">>, AccountId}
           ,{<<"pvt_account_db">>, kazoo_modb:get_modb(AccountId, Year, Month)}
          ],
    Doc = kz_json:set_values(Upd, kz_json:new()),
    kazoo_modb:save_doc(AccountId, Doc, Year, Month).


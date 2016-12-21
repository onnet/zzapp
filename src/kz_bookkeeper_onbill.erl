-module(kz_bookkeeper_onbill).

-export([sync/2]).
-export([is_good_standing/1, is_good_standing/2]).
-export([transactions/3]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).


-export([populate_modb_day_with_fee/4
         ,populate_modb_with_fees/3
        ]).

-include("onbill.hrl").
-include_lib("/opt/kazoo/core/braintree/include/braintree.hrl").
-include_lib("/opt/kazoo/core/kazoo/include/kz_databases.hrl").
-include_lib("/opt/kazoo/core/kazoo_transactions/include/kazoo_transactions.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-spec sync(kz_service_item:items(), ne_binary()) -> 'ok'.
sync(Items, AccountId) ->
  lager:info("IAM sync"),
    Timestamp = kz_util:current_tstamp(),
    {Year, Month, Day} = erlang:date(),
    DailyCountItems = daily_count_items_list(Items, AccountId),
    lager:debug("sync daily count items for: ~p : ~p",[AccountId, DailyCountItems]),
    sync(Timestamp, Year, Month, Day, DailyCountItems, AccountId, 0.0, Items).

sync(_Timestamp, _Year, _Month, _Day, [], _AccountId, Acc, _Items) when Acc == 0.0 ->
    lager:debug("sync Daily fee check NOT NEEDED. Total: ~p",[Acc]),
    'ok';
sync(Timestamp, Year, Month, Day, [], AccountId, Acc, Items) ->
    lager:debug("sync Daily fee calculation finished for ~p. Total: ~p",[AccountId, Acc]),
    DailyFeeId = onbill_bk_util:prepare_dailyfee_doc_name(Year, Month, Day),
    case kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month) of
        {'error', 'not_found'} -> create_dailyfee_doc(Timestamp, Year, Month, Day, Acc, Items, AccountId);
        {'ok', DFDoc} -> maybe_update_dailyfee_doc(Timestamp, Year, Month, Acc, DFDoc, Items, AccountId)
    end,
    'ok';
sync(Timestamp, Year, Month, Day, [ServiceItem|ServiceItems], AccountId, Acc, Items) ->
    JObj = kz_service_item:bookkeeper(<<"onbill">>, ServiceItem),
    case {kz_json:get_value(<<"plan">>, JObj), kz_json:get_value(<<"addon">>, JObj)} of
        {'undefined', _} ->
            lager:debug("sync service item had no plan id: ~p", [ServiceItem]),
            sync(Timestamp, Year, Month, Day, ServiceItems, AccountId, Acc, Items);
        {_, 'undefined'} ->
            lager:debug("sync service item had no add on id: ~p", [ServiceItem]),
            sync(Timestamp, Year, Month, Day, ServiceItems, AccountId, Acc, Items);
        {_PlanId, _AddOnId}->
            ItemCost = calc_item(ServiceItem, AccountId),
            SubTotal = Acc + ItemCost,
            sync(Timestamp, Year, Month, Day, ServiceItems, AccountId, SubTotal, Items)
    end.

-spec calc_item(kz_service_item:item(), ne_binary()) -> number().
calc_item(ServiceItem, AccountId) ->
    try
        Quantity = kz_service_item:quantity(ServiceItem),
        Rate = kz_service_item:rate(ServiceItem),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%   Do not forget to add discount calculations !!!!! %%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        _SingleDiscount = kz_service_item:single_discount(ServiceItem),
        _SingleDiscountRate = kz_service_item:single_discount_rate(ServiceItem),
        _CumulativeDiscount = kz_service_item:cumulative_discount(ServiceItem),
        _CumulativeDiscountRate = kz_service_item:cumulative_discount_rate(ServiceItem),
        ItemCost = Rate * Quantity,
        ItemCost
    catch
        E:R ->
            lager:debug("exception syncing acount: ~p : ~p: ~p", [AccountId, E, R]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, kz_service_item:public_json(ServiceItem)]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, kz_service_item:rate(ServiceItem)]),
            Subj = io_lib:format("OnBill Bookkeeper syncing problem! AccountId: ~p",[AccountId]),
            Msg = io_lib:format("Exception syncing AccountId: ~p <br /> Service Item: ~p <br /> Rate: ~p"
                               ,[AccountId
                                ,kz_service_item:public_json(ServiceItem)
                                ,kz_service_item:rate(ServiceItem)
                                ]),
            kz_notify:system_alert(Subj, Msg, []),
            0.0
    end.

-spec is_good_standing(ne_binary()) -> boolean().
is_good_standing(AccountId) ->
  lager:info("IAM is_good_standing/1: ~p",[wht_util:current_balance(AccountId) > 0]),
    wht_util:current_balance(AccountId) > 0.

-spec is_good_standing(ne_binary(), ne_binary()) -> boolean().
is_good_standing(AccountId, _Status) ->
  lager:info("IAM is_good_standing/1: ~p, Status: ~p",[(wht_util:current_balance(AccountId) > 0), _Status]),
    wht_util:current_balance(AccountId) > 0.

-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
                          {'error', 'not_found'} |
                          {'error', 'unknown_error'}.
transactions(AccountId, From, To) ->
  lager:info("IAM transactions AccountId: ~p, From: ~p, To: ~p",[AccountId, From, To]),
    case kz_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason}=Error -> Error;
        {'ok', _Transactions}=Res -> Res
    end.

-spec subscriptions(ne_binary()) -> atom() | kz_json:objects().
subscriptions(AccountId) ->
  lager:debug("IAM subscriptions/1 call. AccountId: ~p",[AccountId]),
    [kz_json:new()].

-spec commit_transactions(ne_binary(),kz_transactions:kz_transactions()) -> 'ok' | 'error'.
-spec commit_transactions(ne_binary(), kz_transactions:kz_transactions(), integer()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions) ->
    commit_transactions(BillingId, Transactions, 3).

commit_transactions(BillingId, Transactions, Try) when Try > 0 ->
  lager:info("IAM commit_transactions BillingId: ~p, Transactions: ~p, Try: ~p", [BillingId, kz_transactions:to_json(Transactions), Try]),
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, BillingId) of
        {'error', _E} ->
            lager:error("could not open services for ~p : ~p retrying...", [BillingId, _E]),
            commit_transactions(BillingId, Transactions, Try-1);
        {'ok', JObj} ->
  lager:info("IAM commit_transactions JObj: ~p", [JObj]),
            NewTransactions = kz_json:get_value(<<"transactions">>, JObj, [])
                ++ kz_transactions:to_json(Transactions),
            JObj1 = kz_json:set_values([{<<"pvt_dirty">>, 'true'}
                                        ,{<<"pvt_modified">>, kz_util:current_tstamp()}
                                        ,{<<"transactions">>, NewTransactions}
                                       ], JObj),
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, JObj1) of
                {'error', _E} ->
                    lager:error("could not save services for ~p : ~p retrying...", [BillingId, _E]),
                    commit_transactions(BillingId, Transactions, Try-1);
                {'ok', _} ->
                    lager:error("IAM commit_transactions new JObj1 saved: ~p", [JObj1]),
                    'ok'
            end
    end;
commit_transactions(BillingId, _Transactions, _Try) ->
  lager:info("IAM commit_transactions"),
    lager:error("too many attempts writing transaction to services in ~p", [BillingId]),
    'error'.

-spec already_charged(ne_binary() | integer() , integer() | kz_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
  lager:info("IAM already_charged/2 BillingId: ~p, Code: ~p",[BillingId, Code]),
    kz_bookkeeper_braintree:already_charged(BillingId, Code).

-spec charge_transactions(ne_binary(), kz_json:objects()) -> kz_json:objects().
charge_transactions(BillingId, Transactions) ->
  lager:info("IAM charge_transactions/2 BillingId: ~p, Transactions: ~p",[BillingId, Transactions]),
    charge_transactions(BillingId, Transactions, []).

charge_transactions(_, [], FailedTransactionsAcc) ->
  lager:info("IAM charge_transactions/3 []"),
    FailedTransactionsAcc;

charge_transactions(BillingId, [Transaction|Transactions], FailedTransactionsAcc) ->
  lager:info("IAM charge_transactions/3 BillingId: ~p, Transaction: ~p",[BillingId, Transactions]),
    Result = case kz_json:get_value(<<"pvt_code">>, Transaction) of
                 ?CODE_TOPUP -> handle_topup(BillingId, Transaction);
                 _ -> handle_charged_transaction(BillingId, Transaction)
             end,
    charge_transactions(BillingId, Transactions, Result ++ FailedTransactionsAcc).

handle_charged_transaction(AccountId, Transaction) ->
  lager:info("IAM handle_charged_transaction/2 AccountId: ~p, Transaction: ~p",[AccountId, Transaction]),
    %%
    %% already_charged should be enhanced to check not only braintree transactions existance but transactions saved in Couch also
    %%
    case kazoo_modb:save_doc(AccountId, Transaction) of
        {'ok', _} -> [];
        _ -> [Transaction]
    end.

-spec handle_topup(ne_binary(), kz_json:object()) -> proplist().
handle_topup(BillingId, Transaction) ->
    case already_charged(BillingId, ?CODE_TOPUP) of
        'true' ->
            [];
        'false' ->
            Amount = kz_json:get_integer_value(<<"pvt_amount">>, Transaction, 0),
            Props = [{<<"purchase_order">>, ?CODE_TOPUP}],
            BT = braintree_transaction:quick_sale(
                   BillingId
                   ,wht_util:units_to_dollars(Amount)
                   ,Props
                  ),
            Success = handle_quick_sale_response(BT),
            _ = send_topup_notification(Success, BillingId, BT),
            case Success of
                'true' -> [];
                'false' -> [Transaction]
            end
    end.

-spec send_topup_notification(boolean(), ne_binary(), bt_transaction()) -> boolean().
send_topup_notification(Success, BillingId, BtTransaction) ->
    Transaction = braintree_transaction:record_to_json(BtTransaction),
    Amount = wht_util:dollars_to_units(kz_json:get_float_value(<<"amount">>, Transaction, 0.0)),
    Props = [{<<"Account-ID">>, BillingId}
             ,{<<"Amount">>, Amount}
             ,{<<"Success">>, Success}
             ,{<<"Response">>, kz_json:get_value(<<"processor_response_text">>, Transaction)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = case
            kapps_util:amqp_pool_send(
              Props
              ,fun kapi_notifications:publish_topup/1
             )
        of
            'ok' ->
                lager:debug("topup notification sent for ~s", [BillingId]);
            {'error', _R} ->
                lager:error("failed to send topup notification for ~s : ~p",[BillingId, _R])
        end,
    Success.

-spec handle_quick_sale_response(bt_transaction()) -> boolean().
handle_quick_sale_response(BtTransaction) ->
    Transaction = braintree_transaction:record_to_json(BtTransaction),
    RespCode = kz_json:get_value(<<"processor_response_code">>, Transaction, ?CODE_UNKNOWN),
    %% https://www.braintreepayments.com/docs/ruby/reference/processor_responses
    kz_util:to_integer(RespCode) < 2000.

create_dailyfee_doc(Timestamp, Year, Month, Day, Amount, Items, AccountId) ->
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
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], filter_out_items(Items)}
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"daily_calculated_items">>]
            ,filter_out_daily_count_items(Items, AccountId)
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
 
maybe_update_dailyfee_doc(Timestamp, Year, Month, Amount, DFDoc, Items, AccountId) ->
    case (wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month) >
          kz_json:get_number_value(<<"pvt_amount">>, DFDoc, 0)
         ) of
        'true' -> update_dailyfee_doc(Timestamp, Year, Month, Amount, DFDoc, Items, AccountId);
        'false' -> 'ok'
    end.

update_dailyfee_doc(Timestamp, Year, Month, Amount, DFDoc, Items, AccountId) ->
    Upd = [{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month)}
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"all_items">>], filter_out_items(Items)}
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"daily_calculated_items">>]
            ,filter_out_daily_count_items(Items, AccountId)
            }
           ,{[<<"pvt_metadata">>,<<"items_history">>, ?TO_BIN(Timestamp),<<"monthly_amount_of_daily_calculated_items">>]
            ,wht_util:dollars_to_units(Amount)
            }
           ,{<<"pvt_modified">>, Timestamp}
          ],
    NewDoc = kz_json:set_values(Upd, DFDoc),
    kazoo_modb:save_doc(AccountId, NewDoc, Year, Month).

-spec populate_modb_with_fees(ne_binary(), integer(), integer()) -> proplist().
populate_modb_with_fees(AccountId, Year, Month) ->
    LastMonthDay = calendar:last_day_of_the_month(Year, Month),
    [populate_modb_day_with_fee(AccountId, Year, Month, Day) || Day <- lists:seq(1, LastMonthDay)].

-spec populate_modb_day_with_fee(ne_binary(), integer(), integer(), integer()) -> any().
populate_modb_day_with_fee(AccountId, Year, Month, Day) ->
    Timestamp = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{3,0,0}}),
    {CurrYear, CurrMonth, _} = erlang:date(),
    {'ok', ServicesJObj} = case {Year, Month} of
                               {CurrYear, CurrMonth} ->
                                   kz_datamgr:open_doc(<<"services">>, AccountId);
                               _ ->
                                   {Y, M} = onbill_util:next_month(Year, Month),
                                   Modb = kazoo_modb:get_modb(AccountId, Y, M),
                                   kazoo_modb:open_doc(Modb, <<"services_bom">>)
                           end,
    {'ok', Items} = kz_service_plans:create_items(ServicesJObj),
    ItemList = kz_service_items:to_list(Items),
    sync(Timestamp, Year, Month, Day, ItemList, AccountId, 0.0, Items).

daily_count_items_list(Items, AccountId) ->
    [Item || Item <- kz_service_items:to_list(Items)
            ,lists:member(kz_service_item:category(Item)
                         ,kz_json:get_value(<<"pvt_daily_count_categories">>, onbill_util:reseller_vars(AccountId), [])
                         )
    ].

filter_out_daily_count_items(Items, AccountId) ->
    ResellerVars = onbill_util:reseller_vars(AccountId),
    Upd = [{[kz_service_item:category(Item), kz_service_item:item(Item)], kz_service_item:public_json(Item)}
           || Item <- kz_service_items:to_list(Items)
           ,lists:member(kz_service_item:category(Item)
                        ,kz_json:get_value(<<"pvt_daily_count_categories">>, ResellerVars, [])
                        )
           ,kz_service_item:quantity(Item) > 0
    ],
    kz_json:set_values(Upd, kz_json:new()).

filter_out_items(Items) ->
    Upd = [{[kz_service_item:category(Item), kz_service_item:item(Item)], kz_service_item:public_json(Item)}
           || Item <- kz_service_items:to_list(Items)
           ,kz_service_item:quantity(Item) > 0
    ],
    kz_json:set_values(Upd, kz_json:new()).

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
    Timestamp = kz_util:current_tstamp(),
    case onbill_bk_util:max_daily_usage_exceeded(Items, AccountId, Timestamp) of
        {'true', NewMax} ->
            DailyCountItems = onbill_bk_util:select_daily_count_items_list(NewMax, AccountId),
            lager:debug("sync daily count items for: ~p : ~p",[AccountId, DailyCountItems]),
            sync(Timestamp, DailyCountItems, AccountId, 0.0, NewMax, Items);
        'false' ->
            lager:debug("max usage not exceeded, no sync needed for: ~p",[AccountId])
    end.

sync(_Timestamp, [], _AccountId, Acc, _NewMax, _Items) when Acc == 0.0 ->
    lager:debug("sync Daily fee check NOT NEEDED. Total: ~p",[Acc]);
sync(Timestamp, [], AccountId, Acc, NewMax, Items) ->
    onbill_bk_util:save_dailyfee_doc(Timestamp, AccountId, Acc, NewMax, Items),
    lager:debug("sync Daily fee calculation finished for ~p. Total: ~p",[AccountId, Acc]);

sync(Timestamp, [ServiceItem|ServiceItems], AccountId, Acc, NewMax, Items) ->
    ItemCost = calc_item(ServiceItem, AccountId),
    SubTotal = Acc + ItemCost,
    sync(Timestamp, ServiceItems, AccountId, SubTotal, NewMax, Items).

-spec calc_item(kz_service_item:item(), ne_binary()) -> number().
calc_item(ServiceItem, AccountId) ->
    try
        Quantity = kz_json:get_value(<<"quantity">>, ServiceItem),
        Rate = kz_json:get_value(<<"rate">>, ServiceItem),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%   Do not forget to add discount calculations !!!!! %%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   %     _SingleDiscount = kz_service_item:single_discount(ServiceItem),
   %     _SingleDiscountRate = kz_service_item:single_discount_rate(ServiceItem),
   %     _CumulativeDiscount = kz_service_item:cumulative_discount(ServiceItem),
   %     _CumulativeDiscountRate = kz_service_item:cumulative_discount_rate(ServiceItem),
        ItemCost = Rate * Quantity,
        ItemCost
    catch
        E:R ->
            lager:debug("exception syncing acount: ~p : ~p: ~p", [AccountId, E, R]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, ServiceItem]),
            lager:debug("exception syncing acount: ~p service item: ~p", [AccountId, kz_json:get_value(<<"rate">>, ServiceItem)]),
            Subj = io_lib:format("OnBill Bookkeeper syncing problem! AccountId: ~p",[AccountId]),
            Msg = io_lib:format("Exception syncing AccountId: ~p <br /> Service Item: ~p <br /> Rate: ~p"
                               ,[AccountId
                                ,ServiceItem
                                ,kz_json:get_value(<<"rate">>, ServiceItem)
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
    NewMax = onbill_bk_util:select_non_zero_items_json(Items),
    DailyCountItems = onbill_bk_util:select_daily_count_items_list(NewMax, AccountId),
    sync(Timestamp, DailyCountItems, AccountId, 0.0, NewMax, Items).

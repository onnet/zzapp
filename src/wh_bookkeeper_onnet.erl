-module(wh_bookkeeper_onnet).

-export([sync/2]).
-export([is_good_standing/1]).
-export([transactions/3]).
-export([subscriptions/1]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([already_charged/2]).

-include("onbill.hrl").
-include_lib("/opt/kazoo/core/braintree/include/braintree.hrl").
-include_lib("/opt/kazoo/core/whistle/include/wh_databases.hrl").
-include_lib("/opt/kazoo/core/whistle_transactions/include/whistle_transactions.hrl").

-define(TR_DESCRIPTION, <<"braintree transaction">>).

-spec sync(wh_service_item:items(), ne_binary()) -> 'ok'.
sync(Items, AccountId) ->
    ItemList = wh_service_items:to_list(Items),
    sync(ItemList, AccountId, 0.0, Items).

sync([], _AccountId, Acc, _Items) when Acc == 0.0 ->
    lager:debug("sync Daily fee check NOT NEEDED. Total: ~p",[Acc]),
    'ok';
sync([], AccountId, Acc, Items) ->
    lager:debug("sync Daily fee check NEEDED. Total: ~p",[Acc]),
    DailyFeeId = prepare_dailyfee_doc_name(),
    case kazoo_modb:open_doc(AccountId, DailyFeeId) of
        {'error', 'not_found'} -> create_dailyfee_doc(Acc, Items, AccountId);
        {'ok', DFDoc} -> maybe_update_dailyfee_doc(Acc, DFDoc, Items, AccountId)
    end,
    'ok';
sync([ServiceItem|ServiceItems], AccountId, Acc, Items) ->
    JObj = wh_service_item:bookkeeper(<<"onnet">>, ServiceItem),
    case {wh_json:get_value(<<"plan">>, JObj), wh_json:get_value(<<"addon">>, JObj)} of
        {'undefined', _} ->
            lager:debug("sync service item had no plan id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Acc, Items);
        {_, 'undefined'} ->
            lager:debug("sync service item had no add on id: ~p", [ServiceItem]),
            sync(ServiceItems, AccountId, Acc, Items);
        {PlanId, AddOnId}->
            Quantity = wh_service_item:quantity(ServiceItem),
            Rate = wh_service_item:rate(ServiceItem),
            SingleDiscount = wh_service_item:single_discount(ServiceItem),
            SingleDiscountRate = wh_service_item:single_discount_rate(ServiceItem),
            CumulativeDiscount = wh_service_item:cumulative_discount(ServiceItem),
            CumulativeDiscountRate = wh_service_item:cumulative_discount_rate(ServiceItem),
            ItemCost = Rate * Quantity,
            % Will implement discounts later, just a test for now
            SubTotal = Acc + ItemCost,

            lager:debug("IAM sync full service item found: ~p", [ServiceItem]),
            lager:debug("IAM sync full service item PlanId: ~p, AddOnId: ~p", [PlanId, AddOnId]),
            lager:debug("IAM sync full service item Quantity: ~p", [Quantity]),
            lager:debug("IAM sync full service item Rate: ~p", [Rate]),
            lager:debug("IAM sync full service item SingleDiscount: ~p", [SingleDiscount]),
            lager:debug("IAM sync full service item SingleDiscountRate: ~p", [SingleDiscountRate]),
            lager:debug("IAM sync full service item CumulativeDiscount: ~p", [CumulativeDiscount]),
            lager:debug("IAM sync full service item CumulativeDiscountRate: ~p", [CumulativeDiscountRate]),
            lager:debug("IAM sync full service item ItemCost: ~p", [ItemCost]),
            lager:debug("IAM sync full service item SubTotal: ~p", [SubTotal]),

            sync(ServiceItems, AccountId, SubTotal, Items)
    end.

-spec is_good_standing(ne_binary()) -> boolean().
is_good_standing(AccountId) ->
    wht_util:current_balance(AccountId) > 0.

-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', wh_transaction:transactions()} |
                          {'error', 'not_found'} |
                          {'error', 'unknown_error'}.
transactions(AccountId, From, To) ->
    case wh_transactions:fetch_local(AccountId, From, To) of
        {'error', _Reason}=Error -> Error;
        {'ok', _Transactions}=Res -> Res
    end.

-spec subscriptions(ne_binary()) -> atom() | wh_json:objects().
subscriptions(AccountId) ->
    lager:debug("void subscriptions/1 call. AccountId: ~p",[AccountId]),
    [wh_json:new()].

-spec commit_transactions(ne_binary(),wh_transactions:wh_transactions()) -> 'ok' | 'error'.
-spec commit_transactions(ne_binary(), wh_transactions:wh_transactions(), integer()) -> 'ok' | 'error'.
commit_transactions(BillingId, Transactions) ->
    commit_transactions(BillingId, Transactions, 3).

commit_transactions(BillingId, Transactions, Try) when Try > 0 ->
    case kz_datamgr:open_doc(?WH_SERVICES_DB, BillingId) of
        {'error', _E} ->
            lager:error("could not open services for ~p : ~p retrying...", [BillingId, _E]),
            commit_transactions(BillingId, Transactions, Try-1);
        {'ok', JObj} ->
            NewTransactions = wh_json:get_value(<<"transactions">>, JObj, [])
                ++ wh_transactions:to_json(Transactions),
            JObj1 = wh_json:set_values([{<<"pvt_dirty">>, 'true'}
                                        ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                        ,{<<"transactions">>, NewTransactions}
                                       ], JObj),
            case kz_datamgr:save_doc(?WH_SERVICES_DB, JObj1) of
                {'error', _E} ->
                    lager:error("could not save services for ~p : ~p retrying...", [BillingId, _E]),
                    commit_transactions(BillingId, Transactions, Try-1);
                {'ok', _} -> 'ok'
            end
    end;
commit_transactions(BillingId, _Transactions, _Try) ->
    lager:error("too many attempts writing transaction to services in ~p", [BillingId]),
    'error'.

-spec already_charged(ne_binary() | integer() , integer() | wh_json:objects()) -> boolean().
already_charged(BillingId, Code) when is_integer(Code) ->
    wh_bookkeeper_braintree:already_charged(BillingId, Code).

-spec charge_transactions(ne_binary(), wh_json:objects()) -> wh_json:objects().
charge_transactions(BillingId, Transactions) ->
    charge_transactions(BillingId, Transactions, []).

charge_transactions(_, [], FailedTransactionsAcc) ->
    FailedTransactionsAcc;

charge_transactions(BillingId, [Transaction|Transactions], FailedTransactionsAcc) ->
    Result = case wh_json:get_value(<<"pvt_code">>, Transaction) of
                 ?CODE_TOPUP -> handle_topup(BillingId, Transaction);
                 _ -> handle_charged_transaction(BillingId, Transaction)
             end,
    charge_transactions(BillingId, Transactions, Result ++ FailedTransactionsAcc).

handle_charged_transaction(AccountId, Transaction) ->
    %%
    %% already_charged should be enhanced to check not only braintree transactions existance but transactions saved in Couch also
    %%
    case kazoo_modb:save_doc(AccountId, Transaction) of
        {'ok', _} -> [];
        _ -> [Transaction]
    end.

-spec handle_topup(ne_binary(), wh_json:object()) -> proplist().
handle_topup(BillingId, Transaction) ->
    case already_charged(BillingId, ?CODE_TOPUP) of
        'true' ->
            [];
        'false' ->
            Amount = wh_json:get_integer_value(<<"pvt_amount">>, Transaction, 0),
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
    Amount = wht_util:dollars_to_units(wh_json:get_float_value(<<"amount">>, Transaction, 0.0)),
    Props = [{<<"Account-ID">>, BillingId}
             ,{<<"Amount">>, Amount}
             ,{<<"Success">>, Success}
             ,{<<"Response">>, wh_json:get_value(<<"processor_response_text">>, Transaction)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = case
            whapps_util:amqp_pool_send(
              Props
              ,fun wapi_notifications:publish_topup/1
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
    RespCode = wh_json:get_value(<<"processor_response_code">>, Transaction, ?CODE_UNKNOWN),
    %% https://www.braintreepayments.com/docs/ruby/reference/processor_responses
    wh_util:to_integer(RespCode) < 2000.

prepare_dailyfee_doc_name() ->
    {_, M, D} = erlang:date(),
    Month = wh_util:pad_month(M),
    Day = wh_util:pad_month(D),
    <<"dailyfee-", Month/binary, Day/binary>>.

create_dailyfee_doc(Amount, Items, AccountId) ->
    Timestamp = wh_util:current_tstamp(),
    {Year, Month, _} = erlang:date(),
    create_dailyfee_doc(Timestamp, Year, Month, Amount, Items, AccountId).

create_dailyfee_doc(Timestamp, Year, Month, Amount, Items, AccountId) ->
    Updates = [{<<"_id">>, prepare_dailyfee_doc_name()}
               ,{<<"pvt_type">>, <<"debit">>}
               ,{<<"description">>, <<"daily_fee ", (prepare_dailyfee_doc_name())/binary>>}
               ,{<<"pvt_reason">>, <<"daily_fee">>}
               ,{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Year, Month)}
               ,{[<<"pvt_metadata">>,<<"items_history">>,wh_util:to_binary(Timestamp),<<"monthly_amount">>], wht_util:dollars_to_units(Amount)}
               ,{[<<"pvt_metadata">>,<<"items_history">>,wh_util:to_binary(Timestamp)], wh_service_items:public_json(Items)}
               ,{<<"pvt_created">>, Timestamp}
               ,{<<"pvt_modified">>, Timestamp}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_account_db">>, kazoo_modb:get_modb(AccountId)}
              ],
    Doc = wh_json:set_values(Updates, wh_json:new()),
    kazoo_modb:save_doc(AccountId, Doc).
 
maybe_update_dailyfee_doc(Amount, DFDoc, Items, AccountId) ->
    {Y,M,_} = erlang:date(),
    case (wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Y, M) >
          wh_json:get_number_value(<<"pvt_amount">>, DFDoc, 0)
         ) of
        'true' -> update_dailyfee_doc(Amount, DFDoc, Items, AccountId);
        'false' -> 'ok'
    end.

update_dailyfee_doc(Amount, DFDoc, Items, AccountId) ->
    Now = wh_util:current_tstamp(),
    {Y,M,_} = erlang:date(),
    Updates = [{<<"pvt_amount">>, wht_util:dollars_to_units(Amount) div calendar:last_day_of_the_month(Y, M)}
               ,{[<<"pvt_metadata">>,<<"items_history">>,wh_util:to_binary(Now),<<"monthly_amount">>], wht_util:dollars_to_units(Amount)}
               ,{[<<"pvt_metadata">>,<<"items_history">>,wh_util:to_binary(Now)], wh_service_items:public_json(Items)}
               ,{<<"pvt_modified">>, Now}
              ],
    NewDoc = wh_json:set_values(Updates, DFDoc),
    kazoo_modb:save_doc(AccountId, NewDoc).

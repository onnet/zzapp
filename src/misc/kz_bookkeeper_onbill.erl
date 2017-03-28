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

-spec sync(kz_service_item:items(), ne_binary()) -> 'ok'|'delinquent'|'retry'.
sync(Items, AccountId) ->
    case kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded')) of
        'true' ->
            maybe_sync(Items, AccountId);
        'false' ->
            'delinquent'
    end.

-spec maybe_sync(kz_service_item:items(), ne_binary()) -> 'ok'|'delinquent'|'retry'.
maybe_sync(Items, AccountId) ->
    lager:info("IAM attempt to sync AccountId: ~p",[AccountId]), 
    case onbill_util:is_trial_account(AccountId) of
        'true' ->
            CurrentUsage = onbill_bk_util:current_usage_amount_in_units(AccountId),
            CurrentBalance = onbill_util:current_balance(AccountId),
            lager:info("IAM Trial AccountId: ~p, CurrentBalance:~p, CurrentUsage: ~p",[AccountId, CurrentBalance, CurrentUsage]), 
            case CurrentBalance > CurrentUsage of
                'true' ->
                    case onbill_util:transit_to_full_suscription_state(AccountId) of
                        {'ok', _} ->
                            sync(Items, AccountId);
                        _ ->
                            'retry'
                    end;
                'false' ->
                    case onbill_util:trial_has_expired(AccountId) of
                        'true'->
                            onbill_notifications:maybe_send_trial_has_expired_update(AccountId),
                            onbill_bk_util:maybe_cancel_trunk_subscriptions(AccountId),
                            'delinquent';
                        'false' ->
                            'ok'
                    end
            end;
        'false' ->
            maybe_billing_period_starts(Items, AccountId)
    end.

maybe_billing_period_starts(Items, AccountId) ->
    Timestamp = kz_time:current_tstamp(),
    {Year, Month, Day} = onbill_util:period_start_date(AccountId, Timestamp),
    case kazoo_modb:open_doc(AccountId, ?MRC_DOC, Year, Month) of
        {'ok', _} ->
            run_sync(Items, AccountId, Timestamp);
        {'error', 'not_found'} ->
            onbill_bk_util:maybe_issue_previous_billing_period_docs(AccountId, Year, Month, Day),
            lager:debug("monthly_recurring doc not found, trying to create"),
            case onbill_bk_util:process_new_billing_period_mrc(AccountId, Timestamp) of
                {'ok', 'mrc_processed'} -> 
                    run_sync(Items, AccountId, Timestamp);
                {'not_enough_funds', 'trunks_canceled'} ->
                    lager:debug("trunks cancelled due to lack of funds, let's start from the beginning for ~p",[AccountId]),
                    kz_service_sync:sync(AccountId);
                {'not_enough_funds', 'no_trunks_set'} ->
                    onbill_notifications:maybe_send_service_suspend_update(AccountId),
                    'delinquent'
            end
    end.

-spec run_sync(kz_service_item:items(), ne_binary(), gregorian_seconds()) -> 'ok'|'delinquent'|'retry'.
run_sync(Items, AccountId, Timestamp) ->
    case onbill_bk_util:max_daily_usage_exceeded(Items, AccountId, Timestamp) of
        {'true', NewMax, ExcessDets} ->
            lager:debug("sync daily AccountId: ~p; excess details: ~p",[AccountId, ExcessDets]),
            _ = onbill_bk_util:charge_newly_added(AccountId, NewMax, ExcessDets, Timestamp),
            DailyCountItems = onbill_bk_util:select_daily_count_items_list(NewMax, AccountId),
            lager:debug("sync daily AccountId: ~p; daily count items: ~p",[AccountId, DailyCountItems]),
            sync(Timestamp, DailyCountItems, AccountId, NewMax, Items);
        'false' ->
            lager:debug("max usage not exceeded, no sync needed for: ~p",[AccountId])
    end,
    case onbill_util:maybe_administratively_convicted(AccountId) of
        'true' ->
            'delinquent';
        'false' ->
            case onbill_util:maybe_convicted(AccountId) of
                'true' -> 'delinquent';
                'false' -> 'ok'
            end
    end.

sync(_Timestamp, [], _AccountId, _NewMax, _Items) ->
    lager:debug("no daily count items found, daily fee sync not needed.");

sync(Timestamp, ServiceItems, AccountId, NewMax, Items) ->
    case onbill_bk_util:items_amount(ServiceItems, AccountId, 0.0) of
        0.0 ->
            lager:debug("daily fee items have zero cost, no changes needed.");
        ItemsCost ->
            onbill_bk_util:save_dailyfee_doc(Timestamp, AccountId, ItemsCost, NewMax, Items),
            lager:debug("sync Daily fee calculation finished for ~p. Total: ~p",[AccountId, ItemsCost])
    end.

-spec is_good_standing(ne_binary()) -> boolean().
is_good_standing(AccountId) ->
    lager:debug("is_good_standing/1 ~p: ~p",[AccountId, not onbill_util:maybe_convicted(AccountId)]),
    not onbill_util:maybe_convicted(AccountId).

-spec is_good_standing(ne_binary(), ne_binary()) -> boolean().
is_good_standing(_AccountId, Status) ->
    lager:debug("is_good_standing/2 _AccountId ~p: ~p, Status Arg: ~p"
               ,[_AccountId, Status =:= kzd_services:status_good(), Status]),
    Status =:= kzd_services:status_good().

-spec transactions(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                          {'ok', kz_transaction:transactions()} |
                          {'error', 'not_found'} |
                          {'error', 'unknown_error'}.
%transactions(AccountId, From, To) ->
%    lager:info("IAM transactions AccountId: ~p, From: ~p, To: ~p",[AccountId, From, To]),
%    case kz_transactions:fetch_local(AccountId, From, To) of
%        {'error', _Reason}=Error -> Error;
%        {'ok', _Transactions}=Res -> Res
%    end.
transactions(_AccountId, _From, _To) ->
    {'ok', []}.

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
                                        ,{<<"pvt_modified">>, kz_time:current_tstamp()}
                                        ,{<<"transactions">>, NewTransactions}
                                       ], JObj),
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, JObj1) of
                {'error', _E} ->
                    lager:error("could not save services for ~p : ~p retrying...", [BillingId, _E]),
                    commit_transactions(BillingId, Transactions, Try-1);
                {'ok', _} ->
                    lager:error("IAM commit_transactions new JObj1 saved: ~p", [JObj1]),
                    kz_service_sync:sync(BillingId),
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
    {Year, Month, _} = erlang:date(),
    DocId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_time:pad_month(Month), kz_binary:rand_hex(16)),
  lager:info("IAM handle_charged_transaction/2 AccountId: ~p, Transaction: ~p",[AccountId, Transaction]),
    %%
    %% already_charged should be enhanced to check not only braintree transactions existance but transactions saved in Couch also
    %%
    case kazoo_modb:save_doc(AccountId, kz_json:set_value(<<"_id">>, DocId, Transaction)) of
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
             | kz_api:default_headers(?OB_APP_NAME, ?OB_APP_VERSION)
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
    kz_term:to_integer(RespCode) < 2000.

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
    sync(Timestamp, DailyCountItems, AccountId, NewMax, Items).

-module(onbill_bk_util).

-export([max_daily_items_usage/2
        ,max_daily_items_usage/5
        ]).

-include("onbill.hrl").
-include_lib("/opt/kazoo/core/braintree/include/braintree.hrl").
-include_lib("/opt/kazoo/core/kazoo/include/kz_databases.hrl").
-include_lib("/opt/kazoo/core/kazoo_transactions/include/kazoo_transactions.hrl").

-spec max_daily_items_usage(kz_service_item:items(), ne_binary()) -> any().
max_daily_items_usage(_SyncItems, AccountId) ->
    _Timestamp = kz_util:current_tstamp(),
    {Year, Month, Day} = erlang:date(),
    max_daily_items_usage(_SyncItems, AccountId, Year, Month, Day).

-spec max_daily_items_usage(kz_service_item:items(), ne_binary(), integer(), integer(), integer()) -> any().
max_daily_items_usage(_SyncItems, AccountId, Year, Month, Day) ->
    DailyFeeId = prepare_dailyfee_doc_name(Year, Month, Day),
    {'ok', DFDoc} = kazoo_modb:open_doc(AccountId, DailyFeeId, Year, Month),
    lager:info("IAM DFDoc: ~p",[DFDoc]).
 %   case kz_json:get_value([<<"pvt_metadata">>,<<"max_usage">>,<<"all_items">>], DFDoc) of
 %       'undefined' -> set_max_usage(

-spec prepare_dailyfee_doc_name(integer(), integer(), integer()) -> ne_binary().
prepare_dailyfee_doc_name(Y, M, D) ->
    Year = kz_util:to_binary(Y),
    Month = kz_util:pad_month(M),
    Day = kz_util:pad_month(D),
    <<Year/binary, Month/binary, Day/binary, "-dailyfee">>.


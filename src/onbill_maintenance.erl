-module(onbill_maintenance).

-export([populate_modb_day_with_fee/4
         ,populate_modb_with_fees/3
        ]).

populate_modb_with_fees(AccountId, Year, Month) ->
    wh_bookkeeper_onnet:populate_modb_with_fees(wh_util:to_binary(AccountId), Year, Month).

populate_modb_day_with_fee(AccountId, Year, Month, Day) ->
    wh_bookkeeper_onnet:populate_modb_day_with_fee(wh_util:to_binary(AccountId), Year, Month, Day).


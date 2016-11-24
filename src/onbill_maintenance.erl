-module(onbill_maintenance).

-export([populate_modb_day_with_fee/4
         ,populate_modb_with_fees/3
        ]).

-include("onbill.hrl").

-spec populate_modb_with_fees(ne_binary(), integer(), integer()) -> ok.
populate_modb_with_fees(AccountId, Year, Month) ->
    kz_bookkeeper_onbill:populate_modb_with_fees(kz_util:to_binary(AccountId), Year, Month).

-spec populate_modb_day_with_fee(ne_binary(), integer(), integer(), integer()) -> ok.
populate_modb_day_with_fee(AccountId, Year, Month, Day) ->
    kz_bookkeeper_onbill:populate_modb_day_with_fee(kz_util:to_binary(AccountId), Year, Month, Day).


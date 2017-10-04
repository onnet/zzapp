%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_periodic_fees).

-export([reconcile/1]).

-include("/opt/kazoo/core/kazoo_services/src/kazoo_services.hrl").

-define(CATEGORY, <<"periodic_fees">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    _ = onbill_util:maybe_add_design_doc(AccountDb, <<"periodic_fees">>),
    case kz_datamgr:get_results(AccountDb, <<"periodic_fees/crossbar_listing">>) of
        {'error', _R} ->
            lager:debug("unable to get current fees in service: ~p", [_R]),
            Services;
        {'ok', []} ->
            lager:debug("empty results when reconciling ~s", [AccountId]),
            kz_services:reset_category(?CATEGORY, Services);
        {'ok', FeeDocs} ->
            JObjs = count_active_fees(kz_time:now_s(), FeeDocs),
            lager:debug("reconciling ~p fees in ~s: ~p", [length(JObjs), AccountId, JObjs]),
            lists:foldl(fun reconcile_fee/2
                       ,kz_services:reset_category(?CATEGORY, Services)
                       ,JObjs
                       )
    end.

-spec reconcile_fee(kz_json:object(), kz_services:services()) -> kz_services:services().
reconcile_fee(JObj, Services) ->
    Item = kz_json:get_value(<<"key">>, JObj),
    Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),
    lager:debug("reconciling fee ~s to ~p", [Item, Quantity]),
    kz_services:update(?CATEGORY, Item, Quantity, Services).

-spec count_active_fees(gregorian_seconds(), kz_json:object()) -> kz_json:objects().
count_active_fees(LookupTstamp, FeeDocs) ->
    ActiveFees = [kz_json:get_value(<<"value">>, JObj)
                  || JObj <- FeeDocs
                    ,onbill_util:maybe_fee_active(LookupTstamp, JObj)
                 ],
    ServicesList = [kz_json:get_value(<<"service_id">>, Fee) || Fee <- ActiveFees],
    ServicesQtyList = [{kz_json:get_value(<<"service_id">>, Fee), kz_json:get_integer_value(<<"quantity">>, Fee, 1)} || Fee <- ActiveFees],
    [{[{<<"key">>, Service}, {<<"value">>,count(Service, ServicesQtyList)}]} || Service <- lists:usort(ServicesList)].

count(_, []) -> 0;
count(X, [{X,Qty}|XS]) -> Qty + count(X, XS);
count(X, [_|XS]) -> count(X, XS).

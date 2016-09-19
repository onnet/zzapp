%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_periodic_fees).

-export([reconcile/1]).
-export([reconcile/2]).

-include("/opt/kazoo/core/kazoo_services/src/kazoo_services.hrl").


-define(CATEGORY, <<"periodic_fees">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), api_binary() | kz_json:object()) -> kz_services:services().
reconcile(Services) ->
  lager:info("IAM kz_service_fees reconcile1 Services: ~p",[Services]),
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    _ = onbill_util:maybe_add_design_doc(AccountDb, <<"periodic_fees">>),
    ViewOptions = ['reduce'
                  ,'group'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"periodic_fees/usage">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current fees in service: ~p", [_R]),
            Services;
        {'ok', []} ->
            lager:debug("empty results when reconciling ~s", [AccountId]),
            kz_services:reset_category(?CATEGORY, Services);
        {'ok', JObjs} ->
            lager:debug("reconciling ~p fees in ~s: ~p", [length(JObjs), AccountId, JObjs]),
            lists:foldl(fun reconcile_fee/2
                       ,kz_services:reset_category(?CATEGORY, Services)
                       ,JObjs
                       )
    end.

reconcile(Services, 'undefined') ->
  lager:info("IAM kz_service_fees reconcile2 indefined"),
    Services;
reconcile(Services, <<_/binary>> = DeviceType) ->
  lager:info("IAM kz_service_fees reconcile2 DeviceType: ~p",[DeviceType]),
    case kz_services:is_dirty(Services) of
        'true' ->
            lager:debug("doing full reconcile for ~s", [DeviceType]),
            do_reconcile(reconcile(Services), DeviceType);
        'false' ->
            lager:debug("doing partial reconcile for ~s", [DeviceType]),
            do_reconcile(Services, DeviceType)
    end.

-spec do_reconcile(kz_services:services(), ne_binary()) -> kz_services:services().
do_reconcile(Services, DeviceType) ->
    Quantity = kz_services:quantity(?CATEGORY, DeviceType, Services),
    lager:debug("increment ~s.~s to ~p+1", [?CATEGORY, DeviceType, Quantity]),
    kz_services:update(?CATEGORY, DeviceType, Quantity+1, Services).

-spec reconcile_fee(kz_json:object(), kz_services:services()) -> kz_services:services().
reconcile_fee(JObj, Services) ->
    Item = kz_json:get_value(<<"key">>, JObj),
    Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),

    lager:debug("reconciling fee ~s to ~p", [Item, Quantity]),

    kz_services:update(?CATEGORY, Item, Quantity, Services).

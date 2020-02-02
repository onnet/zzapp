-module(onbill_fees).

-export([shape_fees/4
        ,shape_fees/5
        ,per_minute_calls/5
        ,get_period_per_minute_jobjs/4
        ,vatify_amount/3
        ,vatify_amount/4
        ,dates_sequence_reduce/1
        ,days_sequence_reduce/1
        ]).

-include("onbill.hrl").

-spec shape_fees(kz_term:ne_binary(), integer(), integer(), kz_term:ne_binary()) -> 'ok'. 
-spec shape_fees(kz_term:ne_binary(), integer(), integer(), integer(), kz_term:ne_binary()) -> 'ok'. 
-spec shape_fees(kz_term:ne_binary(), integer(), integer(), integer(), kz_json:object(), kz_term:proplist()) -> 'ok'. 
shape_fees(AccountId, Year, Month, Carrier) ->
    shape_fees(AccountId, Year, Month, 1, Carrier).
shape_fees(AccountId, Year, Month, Day, Carrier) ->
    CarrierDoc =  zz_util:carrier_doc(Carrier, AccountId),
    OnbillResellerVars = zz_util:reseller_vars(AccountId),
    shape_fees(AccountId, Year, Month, Day, CarrierDoc, OnbillResellerVars).

shape_fees(AccountId, Year, Month, Day, CarrierDoc, OnbillResellerVars) ->
    FeesList = maybe_monthly_fees(AccountId, CarrierDoc, Year, Month, Day),
    case kz_json:get_value(<<"vat_disposition">>, OnbillResellerVars) of
        <<"netto">> ->
            [enhance_vat_netto(FeeLine, OnbillResellerVars) ++ enhance_extra_codes(FeeLine, OnbillResellerVars) || FeeLine <- FeesList];
        <<"brutto">> ->
            [enhance_vat_brutto(FeeLine, OnbillResellerVars) ++ enhance_extra_codes(FeeLine, OnbillResellerVars) || FeeLine <- FeesList];
        _ ->
            [enhance_no_or_zero_vat(FeeLine, OnbillResellerVars) ++ enhance_extra_codes(FeeLine, OnbillResellerVars) || FeeLine <- FeesList]
    end.

enhance_extra_codes(FeeLine, OnbillResellerVars) ->
    Category = props:get_value(<<"category">>, FeeLine),
    CodesBag = case kz_json:get_value([<<"extra_codes">>, Category], OnbillResellerVars) of
                   'undefined' -> kz_json:get_value([<<"extra_codes">>, <<"default">>], OnbillResellerVars, kz_json:new());
                    FoundBag -> FoundBag
               end,
    [{Key, kz_json:get_value(Key, CodesBag)} || Key <- kz_json:get_keys(CodesBag)].

enhance_no_or_zero_vat(FeeLine, _OnbillResellerVars) ->
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    Discount = props:get_value(<<"discount">>, FeeLine),
    DiscountedCost = Cost - Discount,
    NewValues = [{<<"rate_netto">>, zz_util:price_round(Rate)}
                ,{<<"cost_netto">>, zz_util:price_round(Cost)}
                ,{<<"discount_netto">>, zz_util:price_round(Discount)}
                ,{<<"discounted_cost_netto">>, zz_util:price_round(DiscountedCost)}
                ,{<<"rate_brutto">>, zz_util:price_round(Rate)}
                ,{<<"cost_brutto">>, zz_util:price_round(Cost)}
                ,{<<"discount_brutto">>, zz_util:price_round(Discount)}
                ,{<<"discounted_cost_brutto">>, zz_util:price_round(DiscountedCost)}
                ,{<<"vat_line_total">>, 0.0}
                ,{<<"vat_line_discounted_total">>, 0.0}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_netto(FeeLine, OnbillResellerVars) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillResellerVars),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    Discount = props:get_value(<<"discount">>, FeeLine),
    DiscountedCost = Cost - Discount,
    VatLineTotal = zz_util:price_round(Cost * VatRate / 100),
    VatLineDiscountedTotal = zz_util:price_round(DiscountedCost * VatRate / 100),
    BruttoRate = Rate * (100 + VatRate) / 100,
    BruttoCost = Cost + VatLineTotal,
    BruttoDiscountedCost = DiscountedCost + VatLineDiscountedTotal,
    BruttoDiscount = BruttoCost - BruttoDiscountedCost,
    NewValues = [{<<"rate_netto">>, zz_util:price_round(Rate)}
                ,{<<"cost_netto">>, zz_util:price_round(Cost)}
                ,{<<"discount_netto">>, zz_util:price_round(Discount)}
                ,{<<"discounted_cost_netto">>, zz_util:price_round(DiscountedCost)}
                ,{<<"rate_brutto">>, zz_util:price_round(BruttoRate)}
                ,{<<"cost_brutto">>, zz_util:price_round(BruttoCost)}
                ,{<<"discount_brutto">>, zz_util:price_round(BruttoDiscount)}
                ,{<<"discounted_cost_brutto">>, zz_util:price_round(BruttoDiscountedCost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ,{<<"vat_line_discounted_total">>, VatLineDiscountedTotal}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_brutto(FeeLine, OnbillResellerVars) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillResellerVars),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    Discount = props:get_value(<<"discount">>, FeeLine),
    DiscountedCost = Cost - Discount,
    VatLineTotal = zz_util:price_round(Cost * VatRate / (100 + VatRate)),
    VatLineDiscountedTotal = zz_util:price_round(DiscountedCost * VatRate / (100 + VatRate)),
    NetRate = Rate / (100 + VatRate) * 100,
    NetCost = Cost - VatLineTotal,
    NetDiscountedCost = DiscountedCost - VatLineDiscountedTotal,
    NetDiscount = NetCost - NetDiscountedCost,
    NewValues = [{<<"rate_netto">>, zz_util:price_round(NetRate)}
                ,{<<"cost_netto">>, zz_util:price_round(NetCost)}
                ,{<<"discount_netto">>, zz_util:price_round(NetDiscount)}
                ,{<<"discounted_cost_netto">>, zz_util:price_round(NetDiscountedCost)}
                ,{<<"rate_brutto">>, zz_util:price_round(Rate)}
                ,{<<"cost_brutto">>, zz_util:price_round(Cost)}
                ,{<<"discount_brutto">>, zz_util:price_round(Discount)}
                ,{<<"discounted_cost_brutto">>, zz_util:price_round(DiscountedCost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ,{<<"vat_line_discounted_total">>, VatLineDiscountedTotal}
                ],
    props:set_values(NewValues, FeeLine).

maybe_monthly_fees(AccountId, CarrierDoc, Year, Month, Day) ->
    case zz_util:maybe_main_carrier(CarrierDoc, AccountId) of
        'true' -> monthly_fees(AccountId, Year, Month, Day) ++ process_per_minute_calls(AccountId, Year, Month, Day, CarrierDoc);
        _ -> process_per_minute_calls(AccountId, Year, Month, Day, CarrierDoc)
    end.

monthly_fees(AccountId, Year, Month, Day) ->
    RawTableId = ets:new(erlang:binary_to_atom(<<AccountId/binary, "-raw">>, 'latin1'), [duplicate_bag]),
    ResultTableId = ets:new(erlang:binary_to_atom(<<AccountId/binary, "-result">>, 'latin1'), [bag]),
    {SYear, SMonth, SDay} = zz_util:period_start_date(AccountId, Year, Month, Day),
    {EYear, EMonth, EDay} = zz_util:period_end_date(AccountId, Year, Month, Day),
    case SMonth == EMonth of
        'true' ->
            Modb = kazoo_modb:get_modb(AccountId, Year, Month),
            _ = zz_util:maybe_add_design_doc(Modb, <<"onbills">>),
            case kz_datamgr:get_results(Modb, <<"onbills/daily_fees">>, []) of
                {'error', 'not_found'} ->
                    lager:warning("unable to process monthly fee calculaton for Modb: ~s, skipping", [Modb]);
                {'ok', JObjs } ->
                    [process_daily_fee(JObj, Modb, RawTableId) || JObj <- JObjs] 
            end,
            OneTimeFees = process_one_time_fees(Modb, []);
        'false' ->
            SModb = kazoo_modb:get_modb(AccountId, SYear, SMonth),
            _ = zz_util:maybe_add_design_doc(SModb, <<"onbills">>),
            case kz_datamgr:get_results(SModb
                                       ,<<"onbills/daily_fees">>
                                       ,[{'startkey', ?DAILY_FEE_DOC_NAME(SMonth, SYear, SDay)}])
            of
                {'error', 'not_found'} ->
                    lager:warning("unable to process monthly fee calculaton for Modb: ~s, skipping", [SModb]);
                {'ok', SJObjs } ->
                    [process_daily_fee(JObj, SModb, RawTableId) || JObj <- SJObjs] 
            end,
            EModb = kazoo_modb:get_modb(AccountId, EYear, EMonth),
            _ = zz_util:maybe_add_design_doc(EModb, <<"onbills">>),
            case kz_datamgr:get_results(EModb
                                       ,<<"onbills/daily_fees">>
                                       ,[{'endkey', ?DAILY_FEE_DOC_NAME(EMonth, EYear, EDay)}])
            of
                {'error', 'not_found'} ->
                    lager:warning("unable to process monthly fee calculaton for Modb: ~s, skipping", [EModb]);
                {'ok', EJObjs } ->
                    [process_daily_fee(JObj, EModb, RawTableId) || JObj <- EJObjs] 
            end,
            OneTimeFees = process_one_time_fees(SModb, [{'startkey', ?BEGIN_DAY_TS(SMonth, SYear, SDay)}])
                           ++ process_one_time_fees(EModb, [{'endkey', ?END_DAY_TS(EMonth, EYear, EDay)}])
    end,
    _ = process_ets(RawTableId, ResultTableId),
    ServicesList = ets:tab2list(ResultTableId) ++ OneTimeFees,
    [lager:info("Result Table Line: ~p",[Service]) || Service <- ServicesList],
    ets:delete(RawTableId),
    ets:delete(ResultTableId),
    services_to_proplist(AccountId, ServicesList, EYear, EMonth, EDay).

process_per_minute_calls(AccountId, Year, Month, Day, Carrier) when is_binary(Carrier) ->
    process_per_minute_calls(AccountId, Year, Month, Day, zz_util:carrier_doc(Carrier, AccountId));
process_per_minute_calls(AccountId, Year, Month, Day, CarrierDoc) ->
    ResellerId = zz_util:find_reseller_id(AccountId),
    Timezone = kzd_accounts:timezone(ResellerId),
    JObjs = get_period_per_minute_jobjs(AccountId, Year, Month, Day),
    Regexes = get_per_minute_regexes(AccountId, CarrierDoc),
    {_, CallsTotalSec, CallsTotalSumm} = lists:foldl(fun(X, Acc) -> maybe_count_call(Regexes, X, Acc, Timezone) end, {[], 0,0}, JObjs),
    DaysInPeriod = zz_util:days_in_period(AccountId, Year, Month, Day),
    aggregated_service_to_line({<<"per-minute-voip">>
                               ,<<"description">>
                               ,CallsTotalSumm
                               ,kz_term:to_integer(CallsTotalSec / 60)
                               ,<<"">>
                               ,DaysInPeriod
                               ,kz_json:get_value(<<"per_minute_item_name">>, CarrierDoc, <<"Per minute calls">>)
                               ,<<"per-minute-voip">>
                               ,0.0
                               }
                              ,DaysInPeriod
                              ).

-spec per_minute_calls(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_time:day(), kz_term:ne_binary()) -> ok.
per_minute_calls(AccountId, Year, Month, Day, Carrier) when is_binary(Carrier) ->
    per_minute_calls(AccountId, Year, Month, Day, zz_util:carrier_doc(Carrier, AccountId));
per_minute_calls(AccountId, Year, Month, Day, CarrierDoc) ->
    ResellerId = zz_util:find_reseller_id(AccountId),
    Timezone = kzd_accounts:timezone(ResellerId),
    JObjs = get_period_per_minute_jobjs(AccountId, Year, Month, Day),
    Regexes = get_per_minute_regexes(AccountId, CarrierDoc),
    lists:foldl(fun(X, Acc) -> maybe_count_call(Regexes, X, Acc, Timezone) end, {[], 0,0}, JObjs).

-spec get_period_per_minute_jobjs(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_time:day()) -> kz_json:objects().
get_period_per_minute_jobjs(AccountId, Year, Month, Day) ->
    {SYear, SMonth, SDay} = zz_util:period_start_date(AccountId, Year, Month, Day),
    {EYear, EMonth, EDay} = zz_util:period_end_date(AccountId, Year, Month, Day),
    case SMonth == EMonth of
        'true' ->
            Modb = kazoo_modb:get_modb(AccountId, Year, Month),
            _ = zz_util:maybe_add_design_doc(Modb, <<"onbills">>),
            get_per_minute_jobjs(Modb, []);
        'false' ->
            SModb = kazoo_modb:get_modb(AccountId, SYear, SMonth),
            _ = zz_util:maybe_add_design_doc(SModb, <<"onbills">>),
            EModb = kazoo_modb:get_modb(AccountId, EYear, EMonth),
            _ = zz_util:maybe_add_design_doc(EModb, <<"onbills">>),
            get_per_minute_jobjs(SModb, [{'startkey', ?BEGIN_DAY_TS(SMonth, SYear, SDay)}])
            ++ get_per_minute_jobjs(EModb, [{'endkey', ?END_DAY_TS(EMonth, EYear, EDay)}])
    end.

get_per_minute_jobjs(Modb, Opts) ->
    case kz_datamgr:get_results(Modb, <<"onbills/per_minute_call">>, Opts ++ ['descending']) of
        {'error', 'not_found'} ->
             lager:warning("no per_minute_calls found in Modb: ~s", [Modb]),
             [];
        {'ok', JObjs} -> JObjs
    end.

get_per_minute_regexes(AccountId, CarrierDoc) ->
    case zz_util:maybe_main_carrier(CarrierDoc, AccountId) of
        'true' ->
            Carriers = zz_util:account_carriers_list(AccountId),
            get_other_carriers_regexes(Carriers, AccountId);
        _ -> 
            get_carrier_regexes(CarrierDoc, AccountId)
    end.

get_other_carriers_regexes(Carriers, AccountId) when length(Carriers) > 1 ->
    [get_carrier_regexes(Carrier, AccountId) || Carrier <- Carriers, not zz_util:maybe_main_carrier(Carrier, AccountId)];
get_other_carriers_regexes(_,_) ->
    {<<"^\\d*$">>, <<"^\\d*$">>}.

get_carrier_regexes(Carrier, AccountId) when is_binary(Carrier) ->    
    get_carrier_regexes(zz_util:carrier_doc(Carrier, AccountId), AccountId);    
get_carrier_regexes(CarrierDoc,_) ->    
    {kz_json:get_value(<<"caller_number_regex">>, CarrierDoc, <<"^\\d*$">>)
    ,kz_json:get_value(<<"called_number_regex">>, CarrierDoc, <<"^\\d*$">>)
    }.

maybe_count_call(Regexes, JObj, {JObjs, AccSec, AccAmount}, Timezone) ->
    case maybe_interesting_call(Regexes, JObj) of
        'true' ->
            CallCost = kz_currency:units_to_dollars(kz_json:get_integer_value([<<"value">>,<<"cost">>], JObj, 0)),
            CallDuration = kz_json:get_integer_value([<<"value">>,<<"duration">>], JObj, 0),
            Values = [{[<<"value">>,<<"cost">>], CallCost}
                     ,{[<<"value">>,<<"duration_min">>], CallDuration/60}
                     ,{[<<"value">>,<<"start_datetime">>]
                      ,zz_util:format_datetime_tz(kz_json:get_integer_value([<<"value">>,<<"start">>], JObj), Timezone)
                      }
                     ],
            {[kz_json:set_values(Values, JObj)] ++ JObjs
             ,AccSec + kz_json:get_integer_value([<<"value">>,<<"duration">>], JObj, 0)
             ,AccAmount + CallCost
            };
        _ -> 
            {JObjs, AccSec ,AccAmount}
    end.

maybe_interesting_call(RegexesList, JObj) when is_list(RegexesList) ->
    not lists:foldl(fun(X, Acc) -> maybe_interesting_call(X, JObj) or Acc end, 'false', RegexesList);
maybe_interesting_call({RegexFrom, RegexTo}, JObj) ->
    From = kz_json:get_binary_value([<<"value">>,<<"from">>], JObj, <<>>),
    To = kz_json:get_binary_value([<<"value">>,<<"to">>], JObj, <<>>),
    case {re:run(From, RegexFrom), re:run(To, RegexTo)} of
        {{'match',_}, {'match',_}} -> 'true';
        _ -> 'false'
    end.

process_one_time_fees(Modb, Opts) ->
    case kz_datamgr:get_results(Modb, <<"onbills/one_time_fees">>, Opts) of
        {'error', 'not_found'} ->
             lager:warning("no one time charges found in Modb: ~s", [Modb]),
             [];
        {'ok', JObjs } -> [process_one_time_fee(JObj, Modb) || JObj <- JObjs] 
    end.

process_one_time_fee(JObj, Modb) ->
    {'ok', DFDoc} =  kz_datamgr:open_doc(Modb, kz_json:get_value(<<"id">>, JObj)),
    {Year, Month, Day} = kz_term:to_date(kz_json:get_value(<<"pvt_created">>, DFDoc)),
    DaysInMonth = calendar:last_day_of_the_month(Year, Month),
    Reason = kz_json:get_value(<<"pvt_reason">>, DFDoc),
    Amount = kz_currency:units_to_dollars(kz_json:get_integer_value(<<"pvt_amount">>, DFDoc)),
    {Reason
    ,kz_json:get_value([<<"metadata">>, <<"description">>], DFDoc)
    ,kz_json:get_value([<<"metadata">>, <<"rate">>], DFDoc, Amount) * kz_json:get_value([<<"metadata">>, <<"ratio">>], DFDoc, 1.0)
    ,kz_json:get_value([<<"metadata">>, <<"quantity">>], DFDoc, 1)
    ,zz_util:date_json(Year, Month, Day)
    ,DaysInMonth
    ,one_time_fee_name(Reason, DFDoc)
    ,<<"non_daily_calculated">>
    ,kz_json:get_value([<<"metadata">>, <<"total_discount">>], DFDoc, 0.0)
    }.

one_time_fee_name(<<"number_activation">>, DFDoc) -> 
    kz_json:get_value(<<"number">>, DFDoc);
one_time_fee_name(_, DFDoc) -> 
    kz_json:get_value(<<"description">>, DFDoc).

process_daily_fee(JObj, Modb, RawTableId) ->
    case kz_datamgr:open_doc(Modb, kz_json:get_value(<<"id">>, JObj)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', DFDoc} -> upload_daily_fee_to_ets(DFDoc, RawTableId)
    end.

upload_daily_fee_to_ets(DFDoc, RawTableId) ->
    MaxDailyUsage = kz_json:get_value([<<"pvt_metadata">>,<<"max_usage">>,<<"daily_calculated_items">>],DFDoc),
    <<Year:4/binary, Month:2/binary, Day:2/binary, _/binary>> = kz_json:get_value(<<"_id">>,DFDoc),
    [process_element(kz_json:get_value(ElementKey, MaxDailyUsage), RawTableId, {Year, Month, Day})
     || ElementKey <- kz_json:get_keys(MaxDailyUsage)
     ,kz_json:is_json_object(kz_json:get_value(ElementKey, MaxDailyUsage)) == 'true'
    ].

process_element(Element, RawTableId, Date) ->
    [ets:insert(RawTableId, {category(Unit), item(Unit), rate(Unit), quantity(Unit), Date, name(Unit)})
     || {_, Unit} <- kz_json:to_proplist(Element)
     , quantity(Unit) =/= 0.0
    ].

category(JObj) ->
    kz_json:get_value(<<"category">>, JObj).

item(JObj) ->
    kz_json:get_value(<<"item">>, JObj).

name(JObj) ->
    kz_json:get_value(<<"name">>, JObj).

rate(JObj) ->
    kz_term:to_float(kz_json:get_value(<<"rate">>, JObj)).

quantity(JObj) ->
    kz_term:to_float(kz_json:get_value(<<"quantity">>, JObj)).

process_ets(RawTableId, ResultTableId) ->
    ServiceTypesList = lists:usort(ets:match(RawTableId,{'$1','_','_','_','_','_'})),
    [show_items(ServiceType, RawTableId, ResultTableId) || [ServiceType] <- ServiceTypesList].
    
show_items(ServiceType, RawTableId, ResultTableId) ->
    Items = lists:usort(ets:match(RawTableId,{ServiceType,'$2','_','_','_','_'})),
    [process_ets_item(RawTableId, ResultTableId, ServiceType, Item) || [Item] <- Items].

process_ets_item(RawTableId, ResultTableId, ServiceType, Item) ->
    Prices = lists:usort(ets:match(RawTableId,{ServiceType,Item,'$3','_','_','_'})),
    [handle_ets_item_price(RawTableId, ResultTableId, ServiceType, Item, Price) || [Price] <- Prices].

handle_ets_item_price(RawTableId, ResultTableId, ServiceType, Item, Price) ->
    Quantities = lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,'$4','_','_'})),
    [handle_ets_item_quantity(RawTableId, ResultTableId, ServiceType, Item, Price, Quantity) || [Quantity] <- Quantities].

handle_ets_item_quantity(RawTableId, ResultTableId, ServiceType, Item, Price, Quantity) ->
    Dates = [Date || [Date] <- lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,Quantity,'$5','_'}))],
    [[Name]|_] = lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,Quantity,'_','$6'})),
    lager:info("ETS ServiceType: ~p, Item: ~p, Price: ~p, Quantity: ~p, Dates: ~p, Name: ~p"
              ,[ServiceType, Item, Price, Quantity, dates_sequence_reduce(Dates), Name]
              ),
    ets:insert(ResultTableId, {ServiceType, Item, Price, Quantity, dates_sequence_reduce(Dates), length(Dates), Name, <<"daily_calculated">>, 0.0}).

-spec dates_sequence_reduce(kz_term:proplist()) -> kz_term:proplist().
dates_sequence_reduce(DatesList) ->
    Pairs = lists:usort([{Year,Month} || {Year,Month,_} <- DatesList]),
    [format_days_of_month(Year, Month, DatesList) || {Year,Month} <- Pairs].

format_days_of_month(Year, Month, DatesList) ->
    Days = [?TO_INT(D) || {Y,M,D} <- DatesList, Year == Y  andalso Month == M],
    zz_util:period_json(Year, Month, days_sequence_reduce(Days)).

-spec days_sequence_reduce(kz_term:proplist()) -> kz_term:proplist().
days_sequence_reduce([Digit]) ->
    days_sequence_reduce([Digit], []);
days_sequence_reduce([First,Last]) ->
    days_sequence_reduce([First,Last], []);
days_sequence_reduce(LongList) ->
    days_glue(days_sequence_reduce(LongList, [])).

days_sequence_reduce([Digit], Acc) ->
    Acc ++ [kz_term:to_binary(Digit)];
days_sequence_reduce([First,Last], Acc) ->
    case First+1 == Last of
        'true' -> Acc ++ [<<(kz_term:to_binary(First))/binary,"-", (kz_term:to_binary(Last))/binary>>];
        'false' -> Acc ++ [<<(kz_term:to_binary(First))/binary,",", (kz_term:to_binary(Last))/binary>>]
    end;
days_sequence_reduce([First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [kz_term:to_binary(First)]);
        'true' -> days_sequence_reduce(First, [Next] ++ T, Acc)
    end.
    
days_sequence_reduce(Prev, [], Acc) ->
    days_sequence_reduce([Prev], Acc);
days_sequence_reduce(Prev, [Digit], Acc) ->
    Acc ++ [<<(kz_term:to_binary(Prev))/binary,"-", (kz_term:to_binary(Digit))/binary>>];
days_sequence_reduce(Prev, [First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [<<(kz_term:to_binary(Prev))/binary,"-", (kz_term:to_binary(First))/binary>>]);
        'true' -> days_sequence_reduce(Prev, [Next] ++ T, Acc)
    end.

days_glue(L) ->
    lists:foldl(fun(X,Acc) -> case Acc of <<>> -> X; _ -> <<Acc/binary, ",", X/binary>> end end, <<>>, L).

services_to_proplist(AccountId, ServicesList, Year, Month, Day) ->
    DaysInPeriod = zz_util:days_in_period(AccountId, Year, Month, Day),
    lists:foldl(fun(ServiceLine, Acc) -> service_to_line(ServiceLine, DaysInPeriod, Acc) end, [], ServicesList).

service_to_line({ServiceType, Item, Rate, Quantity, Period, DaysQty, Name, Type, Discount}, DaysInPeriod, Acc) ->
    [[{<<"category">>, ServiceType}
    ,{<<"item">>, Item}
    ,{<<"name">>, Name}
    ,{<<"cost">>, DaysQty / DaysInPeriod * Rate * Quantity}
    ,{<<"rate">>, Rate}
    ,{<<"quantity">>, Quantity}
    ,{<<"days_quantity">>, DaysQty}
    ,{<<"days_in_period">>, DaysInPeriod}
    ,{<<"period">>, Period}
    ,{<<"type">>, Type}
    ,{<<"discount">>, Discount}
    ]] ++ Acc.

aggregated_service_to_line({ServiceType, Item, Cost, Quantity, Period, DaysQty, Name, Type, Discount}, DaysInPeriod) when Cost > 0.0, Quantity > 0.0 ->
    [[{<<"category">>, ServiceType}
    ,{<<"item">>, Item}
    ,{<<"name">>, Name}
    ,{<<"cost">>, Cost}
    ,{<<"rate">>, Cost / Quantity}
    ,{<<"quantity">>, Quantity}
    ,{<<"days_quantity">>, DaysQty}
    ,{<<"days_in_period">>, DaysInPeriod}
    ,{<<"period">>, Period}
    ,{<<"type">>, Type}
    ,{<<"discount">>, Discount}
    ]];
aggregated_service_to_line(_, _) ->
    [].

-spec vatify_amount(kz_term:ne_binary(), number(), kz_json:object()) -> 'ok'.
-spec vatify_amount(kz_term:ne_binary(), number(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
vatify_amount(AmountName, Amount, OnbillResellerVars) ->
  lager:debug("OnbillResellerVars: ~p",[OnbillResellerVars]),
    VatRate = kz_json:get_float_value(<<"vat_rate">>, OnbillResellerVars),
    VatDisposition = kz_json:get_value(<<"vat_disposition">>, OnbillResellerVars),
    vatify_amount(AmountName, Amount, VatRate, VatDisposition).

vatify_amount(AmountName, Netto, VatRate, VatDisposition) when VatDisposition == <<"netto">> ->
    Vat = zz_util:price_round(Netto * VatRate / 100),
    Brutto = Netto + Vat,
    [{<<AmountName/binary, "_netto">>, zz_util:price_round(Netto)}
    ,{<<AmountName/binary, "_brutto">>, zz_util:price_round(Brutto)}
    ,{<<AmountName/binary, "_vat">>, Vat}
    ];
vatify_amount(AmountName, Brutto, VatRate, VatDisposition) when VatDisposition == <<"brutto">> ->
  lager:debug("AmountName: ~p",[AmountName]),
  lager:debug("Brutto: ~p",[Brutto]),
  lager:debug("VatRate: ~p",[VatRate]),
  lager:debug("VatDisposition: ~p",[VatDisposition]),
    Vat = zz_util:price_round(Brutto * VatRate / (100 + VatRate)),
    Netto = zz_util:price_round(Brutto) - Vat,
    [{<<AmountName/binary, "_netto">>, zz_util:price_round(Netto)}
    ,{<<AmountName/binary, "_brutto">>, zz_util:price_round(Brutto)}
    ,{<<AmountName/binary, "_vat">>, Vat}
    ];
vatify_amount(AmountName, Amount, _, _) ->
    [{<<AmountName/binary, "_netto">>, Amount}
    ,{<<AmountName/binary, "_brutto">>, Amount}
    ,{<<AmountName/binary, "_vat">>, 0.0}
    ].

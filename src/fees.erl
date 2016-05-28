-module(fees).

-export([shape_fees/6
        ]).

-include("onbill.hrl").

shape_fees(Modb, CarrierDoc, Year, Month, DaysInMonth, OnbillGlobalVars) ->
    FeesList = maybe_monthly_fees(Modb, CarrierDoc, Year, Month, DaysInMonth),
    case kz_json:get_value(<<"vat_disposition">>, OnbillGlobalVars) of
        <<"netto">> ->
            [enhance_vat_netto(FeeLine, OnbillGlobalVars) ++ enhance_extra_codes(FeeLine, OnbillGlobalVars) || FeeLine <- FeesList];
        <<"brutto">> ->
            [enhance_vat_brutto(FeeLine, OnbillGlobalVars) ++ enhance_extra_codes(FeeLine, OnbillGlobalVars) || FeeLine <- FeesList];
        _ ->
            [enhance_no_or_zero_vat(FeeLine, OnbillGlobalVars) ++ enhance_extra_codes(FeeLine, OnbillGlobalVars) || FeeLine <- FeesList]
    end.

enhance_extra_codes(FeeLine, OnbillGlobalVars) ->
    Category = props:get_value(<<"category">>, FeeLine),
    CodesBag = case kz_json:get_value([<<"extra_codes">>, Category], OnbillGlobalVars) of
                   'undefined' -> kz_json:get_value([<<"extra_codes">>, <<"default">>], OnbillGlobalVars, kz_json:new());
                    FoundBag -> FoundBag
               end,
    [{Key, kz_json:get_value(Key, CodesBag)} || Key <- kz_json:get_keys(CodesBag)].

enhance_no_or_zero_vat(FeeLine, _OnbillGlobalVars) ->
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    NewValues = [{<<"rate_netto">>, onbill_util:price_round(Rate)}
                ,{<<"cost_netto">>, onbill_util:price_round(Cost)}
                ,{<<"rate_brutto">>, onbill_util:price_round(Rate)}
                ,{<<"cost_brutto">>, onbill_util:price_round(Cost)}
                ,{<<"vat_line_total">>, 0.0}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_netto(FeeLine, OnbillGlobalVars) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillGlobalVars),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    VatLineTotal = onbill_util:price_round(Cost * VatRate / 100),
    BruttoCost = Cost + VatLineTotal,
    BruttoRate = Rate * (100 + VatRate) / 100,
    NewValues = [{<<"rate_netto">>, onbill_util:price_round(Rate)}
                ,{<<"cost_netto">>, onbill_util:price_round(Cost)}
                ,{<<"rate_brutto">>, onbill_util:price_round(BruttoRate)}
                ,{<<"cost_brutto">>, onbill_util:price_round(BruttoCost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_brutto(FeeLine, OnbillGlobalVars) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillGlobalVars),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    VatLineTotal = onbill_util:price_round(Cost * VatRate / (100 + VatRate)),
    NetCost = Cost - VatLineTotal,
    NetRate = Rate / (100 + VatRate) * 100,
    NewValues = [{<<"rate_netto">>, onbill_util:price_round(NetRate)}
                ,{<<"cost_netto">>, onbill_util:price_round(NetCost)}
                ,{<<"rate_brutto">>, onbill_util:price_round(Rate)}
                ,{<<"cost_brutto">>, onbill_util:price_round(Cost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ],
    props:set_values(NewValues, FeeLine).

maybe_monthly_fees(Modb, CarrierDoc, Year, Month, DaysInMonth) ->
    case maybe_main_carrier(CarrierDoc) of
        'true' -> monthly_fees(Modb, Year, Month, DaysInMonth) ++ process_per_minute_calls(Modb, CarrierDoc, Year, Month, DaysInMonth);
        _ -> process_per_minute_calls(Modb, CarrierDoc, Year, Month, DaysInMonth)
    end.

maybe_main_carrier(CarrierDoc) ->
% {_, Year, Month} = kazoo_modb_util:split_account_mod(Db)
    case kz_json:get_value(<<"carrier_type">>, CarrierDoc) of
        <<"main">> -> 'true';
        _ -> 'false'
    end.
 
monthly_fees(Modb, Year, Month, DaysInMonth) ->
    RawModb = kz_util:format_account_modb(Modb, 'raw'),
    _ = onbill_util:maybe_add_design_doc(Modb),
    RawTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-raw">>, 'latin1'), [duplicate_bag]),
    ResultTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-result">>, 'latin1'), [bag]),
    case kz_datamgr:get_results(Modb, <<"onbills/daily_fees">>, []) of
        {'error', 'not_found'} -> lager:warning("unable to process monthly fee calculaton for Modb: ~s", [Modb]);
        {'ok', JObjs } -> [process_daily_fee(JObj, Modb, RawTableId) || JObj <- JObjs] 
    end,
    _ = process_ets(RawTableId, ResultTableId),
    ServicesList = ets:tab2list(ResultTableId) ++ process_one_time_fees(Modb),
    [lager:info("Result Table Line: ~p",[Service]) || Service <- ServicesList],
    ets:delete(RawTableId),
    ets:delete(ResultTableId),
    services_to_proplist(ServicesList, Year, Month, DaysInMonth).

process_per_minute_calls(Modb, CarrierDoc, Year, Month, DaysInMonth) ->
    case kz_datamgr:get_results(Modb, <<"onbills/per_minute_call">>, []) of
        {'error', 'not_found'} ->
             lager:warning("no per_minute_calls found in Modb: ~s", [Modb]),
             [];
        {'ok', JObjs } ->
            RegexFrom = kz_json:get_value(<<"caller_number_regex">>, CarrierDoc, <<"^\\d*$">>),
            RegexTo = kz_json:get_value(<<"called_number_regex">>, CarrierDoc, <<"^\\d*$">>),
            {CallsTotalSec, CallsTotalSumm} = lists:foldl(fun(X, Acc) -> maybe_count_call(RegexFrom, RegexTo, X, Acc) end, {0,0}, JObjs),
            aggregated_service_to_line({<<"per-minute-voip">>
                               ,<<"description">>
                               ,wht_util:units_to_dollars(CallsTotalSumm)
                               ,kz_util:to_integer(CallsTotalSec / 60)
                               ,<<"">>
                               ,DaysInMonth
                               ,kz_json:get_value(<<"per_minute_item_name">>, CarrierDoc, <<"Per minute calls">>)
                               }
                               ,Year
                               ,Month
                               ,DaysInMonth
                              )
    end.

maybe_count_call(RegexFrom, RegexTo, JObj, {ASec, AAmount}) ->
    case maybe_interesting_call(RegexFrom, RegexTo, JObj) of
        'true' ->
            {ASec + kz_json:get_integer_value([<<"value">>,<<"duration">>], JObj, 0)
             ,AAmount + kz_json:get_integer_value([<<"value">>,<<"cost">>], JObj, 0)
            };
        _ -> 
            {ASec ,AAmount}
    end.

maybe_interesting_call(RegexFrom, RegexTo, JObj) ->
    From = kz_json:get_binary_value([<<"value">>,<<"from">>], JObj, <<>>),
    To = kz_json:get_binary_value([<<"value">>,<<"to">>], JObj, <<>>),
    case {re:run(From, RegexFrom), re:run(To, RegexTo)} of
        {{'match',_}, {'match',_}} -> 'true';
        _ -> 'false'
    end.

process_one_time_fees(Modb) ->
    case kz_datamgr:get_results(Modb, <<"onbills/one_time_fees">>, []) of
        {'error', 'not_found'} ->
             lager:warning("no one time charges found in Modb: ~s", [Modb]),
             [];
        {'ok', JObjs } -> [process_one_time_fee(JObj, Modb) || JObj <- JObjs] 
    end.

process_one_time_fee(JObj, Modb) ->
    {'ok', DFDoc} =  kz_datamgr:open_doc(Modb, kz_json:get_value(<<"id">>, JObj)),
    {Year, Month, Day} = kz_util:to_date(kz_json:get_value(<<"pvt_created">>, DFDoc)),
    DaysInMonth = calendar:last_day_of_the_month(Year, Month),
    {kz_json:get_value(<<"pvt_reason">>, DFDoc)
     ,kz_json:get_value(<<"description">>, DFDoc)
     ,wht_util:units_to_dollars(kz_json:get_integer_value(<<"pvt_amount">>, DFDoc))
     ,1.0
     ,kz_util:to_binary(Day)
     ,DaysInMonth
     ,one_time_fee_name(DFDoc)
    }.

one_time_fee_name(DFDoc) -> 
    kz_json:get_value(<<"description">>, DFDoc).

process_daily_fee(JObj, Modb, RawTableId) ->
    case kz_datamgr:open_doc(Modb, kz_json:get_value(<<"id">>, JObj)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', DFDoc} -> upload_daily_fee_to_ets(DFDoc, RawTableId)
    end.

upload_daily_fee_to_ets(DFDoc, RawTableId) ->
    ItemsList = kz_json:get_value([<<"pvt_metadata">>, <<"items_history">>],DFDoc),
    [ItemTs|_] = lists:reverse(lists:sort(kz_json:get_keys(ItemsList))),
    Item = kz_json:get_value(ItemTs, ItemsList),
    {{_,_,Day},_} = calendar:gregorian_seconds_to_datetime(kz_util:to_integer(ItemTs)),
    [process_element(kz_json:get_value(ElementKey, Item), RawTableId, Day) || ElementKey <- kz_json:get_keys(Item)
     ,kz_json:is_json_object(kz_json:get_value(ElementKey, Item)) == 'true'
    ].

process_element(Element, RawTableId, Day) ->
    [ets:insert(RawTableId, {category(Unit), item(Unit), rate(Unit), quantity(Unit), Day, name(Unit)})
     || {_, Unit} <- kz_json:to_proplist(Element)
     , quantity(Unit) =/= 0.0
    ].

category(Unit) ->
    kz_json:get_value(<<"category">>, Unit).

item(Unit) ->
    kz_json:get_value(<<"item">>, Unit).

name(Unit) ->
    kz_json:get_value(<<"name">>, Unit).

rate(Unit) ->
    kz_util:to_float(kz_json:get_value(<<"rate">>, Unit)).

quantity(Unit) ->
    kz_util:to_float(kz_json:get_value(<<"quantity">>, Unit)).

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
    Days = [Day || [Day] <- lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,Quantity,'$5','_'}))],
    [[Name]|_] = lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,Quantity,'_','$6'})),
    lager:info("ETS ServiceType: ~p, Item: ~p, Price: ~p, Quantity: ~p, Days: ~p, Name: ~p",[ServiceType, Item, Price, Quantity, days_sequence_reduce(Days), Name]),
    ets:insert(ResultTableId, {ServiceType, Item, Price, Quantity, days_sequence_reduce(Days), length(Days), Name}).

days_sequence_reduce([Digit]) ->
    days_sequence_reduce([Digit], []);
days_sequence_reduce([First,Last]) ->
    days_sequence_reduce([First,Last], []);
days_sequence_reduce(LongList) ->
    days_glue(days_sequence_reduce(LongList, [])).

days_sequence_reduce([Digit], Acc) ->
    Acc ++ [kz_util:to_binary(Digit)];
days_sequence_reduce([First,Last], Acc) ->
    case First+1 == Last of
        'true' -> Acc ++ [<<(kz_util:to_binary(First))/binary,"-", (kz_util:to_binary(Last))/binary>>];
        'false' -> Acc ++ [<<(kz_util:to_binary(First))/binary,",", (kz_util:to_binary(Last))/binary>>]
    end;
days_sequence_reduce([First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [kz_util:to_binary(First)]);
        'true' -> days_sequence_reduce(First, [Next] ++ T, Acc)
    end.
    
days_sequence_reduce(Prev, [], Acc) ->
    days_sequence_reduce([Prev], Acc);
days_sequence_reduce(Prev, [Digit], Acc) ->
    Acc ++ [<<(kz_util:to_binary(Prev))/binary,"-", (kz_util:to_binary(Digit))/binary>>];
days_sequence_reduce(Prev, [First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [<<(kz_util:to_binary(Prev))/binary,"-", (kz_util:to_binary(First))/binary>>]);
        'true' -> days_sequence_reduce(Prev, [Next] ++ T, Acc)
    end.

days_glue(L) ->
    lists:foldl(fun(X,Acc) -> case Acc of <<>> -> X; _ -> <<Acc/binary, ",", X/binary>> end end, <<>>, L).

services_to_proplist(ServicesList, Year, Month, DaysInMonth) ->
    lists:foldl(fun(ServiceLine, Acc) -> service_to_line(ServiceLine, Year, Month, DaysInMonth, Acc) end, [], ServicesList).

service_to_line({ServiceType, Item, Price, Quantity, Period, DaysQty, Name}, Year, Month, DaysInMonth, Acc) ->
    [[{<<"category">>, ServiceType}
    ,{<<"item">>, Item}
    ,{<<"name">>, Name}
    ,{<<"cost">>, DaysQty / DaysInMonth * Price * Quantity}
    ,{<<"rate">>, Price}
    ,{<<"quantity">>, Quantity}
    ,{<<"period">>, Period}
    ,{<<"days_quantity">>, DaysQty}
    ,{<<"days_in_month">>, DaysInMonth}
    ,{<<"month">>, Month}
    ,{<<"month_pad">>, kz_util:pad_month(Month)}
    ,{<<"year">>, Year}
    ]] ++ Acc.

aggregated_service_to_line({ServiceType, Item, Cost, Quantity, Period, DaysQty, Name}, Year, Month, DaysInMonth) when Cost > 0.0, Quantity > 0.0 ->
    [[{<<"category">>, ServiceType}
    ,{<<"item">>, Item}
    ,{<<"name">>, Name}
    ,{<<"cost">>, Cost}
    ,{<<"rate">>, Cost / Quantity}
    ,{<<"quantity">>, Quantity}
    ,{<<"period">>, Period}
    ,{<<"days_quantity">>, DaysQty}
    ,{<<"days_in_month">>, DaysInMonth}
    ,{<<"month">>, Month}
    ,{<<"month_pad">>, kz_util:pad_month(Month)}
    ,{<<"year">>, Year}
    ]];
aggregated_service_to_line(_, _, _, _) ->
    [].

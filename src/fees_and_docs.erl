-module(fees_and_docs).

-export([create_pdf/4
         ,save_pdf/6
         ,monthly_fees/1
         ,days_sequence_reduce/1
         ,generate_docs/3
        ]).

-include("onbill.hrl").

get_template(TemplateId, Carrier) ->
    case kz_datamgr:fetch_attachment(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier), <<(kz_util:to_binary(TemplateId))/binary, ".tpl">>) of
        {'ok', Template} -> Template;
        {error, not_found} ->
            Template = default_template(TemplateId),
            case kz_datamgr:open_doc(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier)) of
                {'ok', _} ->
                    'ok';
                {'error', 'not_found'} ->
                    NewDoc = kz_json:set_values([{<<"_id">>, ?MOD_CONFIG_TEMLATES(Carrier)}
                                                 ,{<<"carrier_name">>,<<"this_doc_carrier_Name">>}
                                                ]
                                                ,kz_json:new()),
                    kz_datamgr:ensure_saved(?SYSTEM_CONFIG_DB, NewDoc)
            end,
            kz_datamgr:put_attachment(?SYSTEM_CONFIG_DB
                                     ,?MOD_CONFIG_TEMLATES(Carrier)
                                     ,<<(kz_util:to_binary(TemplateId))/binary, ".tpl">>
                                     ,Template
                                     ,[{'content_type', <<"text/html">>}]
                                    ),
            Template
    end.

default_template(TemplateId) ->
    FilePath = <<"applications/onbill/priv/templates/ru/", (kz_util:to_binary(TemplateId))/binary, ".tpl">>,
    {'ok', Data} = file:read_file(FilePath),
    Data.

prepare_tpl(Vars, TemplateId, Carrier) ->
    ErlyMod = kz_util:to_atom(TemplateId),
    try erlydtl:compile_template(get_template(TemplateId, Carrier), ErlyMod, [{'out_dir', 'false'},'return']) of
        {ok, ErlyMod} -> render_tpl(ErlyMod, Vars);
        {ok, ErlyMod,[]} -> render_tpl(ErlyMod, Vars); 
        {'ok', ErlyMod, Warnings} ->
            lager:debug("compiling template ~p produced warnings: ~p", [TemplateId, Warnings]),
            render_tpl(ErlyMod, Vars)
    catch
        _E:_R ->
            lager:debug("exception compiling ~p template: ~s: ~p", [TemplateId, _E, _R]),
            <<"Error template compilation">>
    end.

render_tpl(ErlyMod, Vars) ->
    {'ok', IoList} = ErlyMod:render(Vars),
    code:purge(ErlyMod),
    code:delete(ErlyMod),
    erlang:iolist_to_binary(IoList).

create_pdf(Vars, TemplateId, Carrier, AccountId) ->
    Rand = kz_util:rand_hex_binary(5),
    Prefix = <<AccountId/binary, "-", (kz_util:to_binary(TemplateId))/binary, "-", Rand/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),
    HTMLTpl = prepare_tpl(Vars, TemplateId, Carrier),
    file:write_file(HTMLFile, HTMLTpl),
    Cmd = <<(?HTML_TO_PDF(TemplateId))/binary, " ", HTMLFile/binary, " ", PDFFile/binary>>,
    case os:cmd(kz_util:to_list(Cmd)) of
        [] -> file:read_file(PDFFile);
        "\n" -> file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

save_pdf(Vars, TemplateId, Carrier, AccountId, Year, Month) ->
    {'ok', PDF_Data} = create_pdf(Vars, TemplateId, Carrier, AccountId),
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    NewDoc = case kz_datamgr:open_doc(Modb, kz_util:to_binary(TemplateId)) of
        {ok, Doc} -> kz_json:set_values(Vars, Doc);
        {'error', 'not_found'} -> kz_json:set_values(Vars ++ [{<<"_id">>, kz_util:to_binary(TemplateId)}, {<<"pvt_type">>, ?ONBILL_DOC}], kz_json:new()) 
    end,
    kz_datamgr:ensure_saved(Modb, NewDoc),
    kz_datamgr:put_attachment(Modb
                             ,kz_util:to_binary(TemplateId)
                             ,<<(kz_util:to_binary(TemplateId))/binary, ".pdf">>
                             ,PDF_Data
                             ,[{'content_type', <<"application/pdf">>}]
                            ),
    kz_datamgr:flush_cache_doc(Modb, NewDoc).

generate_docs(AccountId, Year, Month) ->
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    Carrier = <<"onnet">>,
    {'ok', TplDoc} =  kz_datamgr:open_doc(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier)),
    {'ok', OnbillCfg} =  kz_datamgr:open_doc(?ONBILL_DB, ?ONBILL_CONFIG),
    {'ok', AccOnbillDoc} =  kz_datamgr:open_doc(?ONBILL_DB, AccountId),
    Docs = ['invoice', 'act'],
    VatUpdatedFeesList = enhance_fees(monthly_fees(Modb), OnbillCfg),
    {TotalNetto, TotalVAT, TotalBrutto} = lists:foldl(fun(X, {TN_Acc, VAT_Acc, TB_Acc}) ->
                                                        {TN_Acc + props:get_value(<<"cost_netto">>, X)
                                                         ,VAT_Acc + props:get_value(<<"vat_line_total">>, X)
                                                         ,TB_Acc + props:get_value(<<"cost_brutto">>, X)
                                                        }
                                                      end
                                                      ,{0,0,0}
                                                      ,VatUpdatedFeesList
                                                     ),
    [TotalBruttoDiv, TotalBruttoRem] = binary:split(float_to_binary(TotalBrutto,[{decimals,2}]), <<".">>),
    [TotalVatDiv, TotalVatRem] = binary:split(float_to_binary(TotalVAT,[{decimals,2}]), <<".">>),
    Vars = [{<<"monthly_fees">>, VatUpdatedFeesList}
           ,{<<"account_addr">>, address_to_line(AccOnbillDoc)}
           ,{<<"total_netto">>, price_round(TotalNetto)}
           ,{<<"total_vat">>, price_round(TotalVAT)}
           ,{<<"total_vat_div">>, unicode:characters_to_binary(amount_into_words:render(TotalVatDiv), unicode, utf8)}
           ,{<<"total_vat_rem">>, unicode:characters_to_binary(amount_into_words:render(TotalVatRem), unicode, utf8)}
           ,{<<"total_brutto">>, price_round(TotalBrutto)}
           ,{<<"total_brutto_div">>, unicode:characters_to_binary(amount_into_words:render(TotalBruttoDiv), unicode, utf8)}
           ,{<<"total_brutto_rem">>, unicode:characters_to_binary(amount_into_words:render(TotalBruttoRem), unicode, utf8)}
           ,{<<"doc_number">>, <<"13">>}
           ,{<<"vat_rate">>, kz_json:get_value(<<"vat_rate">>, OnbillCfg, 0.0)}
           ,{<<"currency1">>, kz_json:get_value(<<"currency1">>, OnbillCfg)}
           ,{<<"agrm_num">>, kz_json:get_value([<<"agrm">>, Carrier, <<"number">>], AccOnbillDoc)}
           ,{<<"agrm_date">>, kz_json:get_value([<<"agrm">>, Carrier, <<"date">>], AccOnbillDoc)}
           ,{<<"doc_date">>, <<"31.",(kz_util:pad_month(Month))/binary,".",(kz_util:to_binary(Year))/binary>>}
           ,{<<"start_date">>, <<"01.",(kz_util:pad_month(Month))/binary,".",(kz_util:to_binary(Year))/binary>>}
           ,{<<"end_date">>, <<(kz_util:to_binary(calendar:last_day_of_the_month(Year, Month)))/binary,".",(kz_util:pad_month(Month))/binary,".",(kz_util:to_binary(Year))/binary>>}
           ] 
           ++ [{Key, kz_json:get_value(Key, TplDoc)} || Key <- kz_json:get_keys(TplDoc), filter_vars(Key)]
           ++ [{Key, kz_json:get_value(Key, AccOnbillDoc)} || Key <- kz_json:get_keys(AccOnbillDoc), filter_vars(Key)],
    [save_pdf(Vars, TemplateId, Carrier, AccountId, Year, Month) || TemplateId <- Docs].

enhance_fees(FeesList, OnbillCfg) ->
    case kz_json:get_value(<<"vat_disposition">>, OnbillCfg) of
        <<"netto">> ->
            [enhance_vat_netto(FeeLine, OnbillCfg) ++ enhance_extra_codes(FeeLine, OnbillCfg) || FeeLine <- FeesList];
        <<"brutto">> ->
            [enhance_vat_brutto(FeeLine, OnbillCfg) ++ enhance_extra_codes(FeeLine, OnbillCfg) || FeeLine <- FeesList];
        _ ->
            [enhance_no_or_zero_vat(FeeLine, OnbillCfg) ++ enhance_extra_codes(FeeLine, OnbillCfg) || FeeLine <- FeesList]
    end.

enhance_extra_codes(FeeLine, OnbillCfg) ->
    Category = props:get_value(<<"category">>, FeeLine),
    CodesBag = case kz_json:get_value([<<"extra_codes">>, Category], OnbillCfg) of
                   'undefined' -> kz_json:get_value([<<"extra_codes">>, <<"default">>], OnbillCfg, kz_json:new());
                    FoundBag -> FoundBag
               end,
    [{Key, kz_json:get_value(Key, CodesBag)} || Key <- kz_json:get_keys(CodesBag)].

enhance_no_or_zero_vat(FeeLine, _OnbillCfg) ->
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    NewValues = [{<<"rate_netto">>, price_round(Rate)}
                ,{<<"cost_netto">>, price_round(Cost)}
                ,{<<"rate_brutto">>, price_round(Rate)}
                ,{<<"cost_brutto">>, price_round(Cost)}
                ,{<<"vat_line_total">>, 0.0}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_netto(FeeLine, OnbillCfg) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillCfg),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    VatLineTotal = price_round(Cost * VatRate / 100),
    BruttoCost = Cost + VatLineTotal,
    BruttoRate = Rate * (100 + VatRate) / 100,
    NewValues = [{<<"rate_netto">>, price_round(Rate)}
                ,{<<"cost_netto">>, price_round(Cost)}
                ,{<<"rate_brutto">>, price_round(BruttoRate)}
                ,{<<"cost_brutto">>, price_round(BruttoCost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ],
    props:set_values(NewValues, FeeLine).

enhance_vat_brutto(FeeLine, OnbillCfg) ->
    VatRate = kz_json:get_value(<<"vat_rate">>, OnbillCfg),
    Rate = props:get_value(<<"rate">>, FeeLine),
    Cost = props:get_value(<<"cost">>, FeeLine),
    VatLineTotal = price_round(Cost * VatRate / (100 + VatRate)),
    NetCost = Cost - VatLineTotal,
    NetRate = Rate / (100 + VatRate) * 100,
    NewValues = [{<<"rate_netto">>, price_round(NetRate)}
                ,{<<"cost_netto">>, price_round(NetCost)}
                ,{<<"rate_brutto">>, price_round(Rate)}
                ,{<<"cost_brutto">>, price_round(Cost)}
                ,{<<"vat_line_total">>, VatLineTotal}
                ],
    props:set_values(NewValues, FeeLine).

price_round(Price) ->
    round(Price * 100) / 100.

filter_vars(<<"_", _/binary>>) -> 'false';
filter_vars(<<_/binary>>) -> 'true'.

address_to_line(JObj) ->
    BillingAddrLinnes = kz_json:get_value(<<"billing_address">>, JObj, kz_json:new()),
    {Keys, _} = kz_json:get_values(BillingAddrLinnes),
    address_join([Line || Line <- Keys, Line =/= <<>>], <<", ">>).

address_join([], _Sep) ->
  <<>>;
address_join([Part], _Sep) ->
  Part;
address_join([Head|Tail], Sep) ->
  lists:foldl(fun (Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end, Head, Tail).

monthly_fees(Db) ->
    {_, Year, Month} = kazoo_modb_util:split_account_mod(Db),
    DaysInMonth = calendar:last_day_of_the_month(Year, Month),
    Modb = kz_util:format_account_modb(Db, 'encoded'),
    RawModb = kz_util:format_account_modb(Db, 'raw'),
    _ = onnet_util:maybe_add_design_doc(Modb),
    RawTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-raw">>, 'latin1'), [duplicate_bag]),
    ResultTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-result">>, 'latin1'), [bag]),
    case kz_datamgr:get_results(Modb, <<"onbills/daily_fees">>, []) of
        {'error', 'not_found'} -> lager:warning("unable to process monthly fee calculaton for Modb: ~s", [Modb]);
        {'ok', JObjs } -> [process_daily_fee(JObj, Modb, RawTableId) || JObj <- JObjs] 
    end,
    _ = process_ets(RawTableId, ResultTableId),
    ServicesList = ets:tab2list(ResultTableId) ++ process_one_time_fees(Db),
    [lager:info("Result Table Line: ~p",[Service]) || Service <- ServicesList],
    ets:delete(RawTableId),
    ets:delete(ResultTableId),
    services_to_proplist(ServicesList, Year, Month, DaysInMonth).

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

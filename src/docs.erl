-module(docs).

-export([generate_docs/3
        ,per_minute_reports/3
        ]).

-include("onbill.hrl").

get_template(TemplateId, Carrier, AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    CountryOfResidence = onbill_util:reseller_country_of_residence(AccountId),
    DbName = kz_util:format_account_id(ResellerId,'encoded'),
    case kz_datamgr:fetch_attachment(DbName, ?CARRIER_DOC(Carrier), <<(?DOC_NAME_FORMAT(Carrier, TemplateId))/binary, ".tpl">>) of
        {'ok', Template} -> Template;
        {error, not_found} ->
            Template = default_template(TemplateId, Carrier, CountryOfResidence),
            case kz_datamgr:open_doc(DbName, ?CARRIER_DOC(Carrier)) of
                {'ok', _} ->
                    'ok';
                {'error', 'not_found'} ->
                    NewDoc = kz_json:set_values([{<<"_id">>, ?CARRIER_DOC(Carrier)}
                                                 ,{<<"called_number_regex">>,<<"^\\d*$">>}
                                                 ,{<<"callee_number_regex">>,<<"^\\d*$">>}
                                                ]
                                                ,kz_json:new()),
                    kz_datamgr:ensure_saved(DbName, NewDoc)
            end,
            kz_datamgr:put_attachment(DbName
                                     ,?CARRIER_DOC(Carrier)
                                     ,<<(?DOC_NAME_FORMAT(Carrier, TemplateId))/binary, ".tpl">>
                                     ,Template
                                     ,[{'content_type', <<"text/html">>}]
                                    ),
            Template
    end.

default_template(TemplateId, Carrier, CountryOfResidence) ->
    CarrierFilePath = <<"applications/onbill/priv/templates/"
                       ,CountryOfResidence/binary
                       ,"/"
                       ,(?DOC_NAME_FORMAT(Carrier, TemplateId))/binary
                       ,".tpl">>,
    case file:read_file(CarrierFilePath) of
        {'ok', CarrierData} -> CarrierData;
        _ ->
            FilePath = <<"applications/onbill/priv/templates/"
                        ,CountryOfResidence/binary
                        ,"/"
                        ,TemplateId/binary
                        ,".tpl">>,
            {'ok', Data} = file:read_file(FilePath),
            Data
    end.

prepare_tpl(Vars, TemplateId, Carrier, AccountId) ->
    ErlyMod = erlang:binary_to_atom(?DOC_NAME_FORMAT(Carrier, TemplateId), 'latin1'),
    try erlydtl:compile_template(get_template(TemplateId, Carrier, AccountId), ErlyMod, [{'out_dir', 'false'},'return']) of
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
    Prefix = <<AccountId/binary, "-", (?DOC_NAME_FORMAT(Carrier, TemplateId))/binary, "-", Rand/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),
    HTMLTpl = prepare_tpl(Vars, TemplateId, Carrier, AccountId),
    file:write_file(HTMLFile, HTMLTpl),
    Cmd = <<?HTML_TO_PDF/binary, " ", HTMLFile/binary, " ", PDFFile/binary>>,
    case os:cmd(kz_util:to_list(Cmd)) of
        [] ->
            file:read_file(PDFFile);
        "\n" ->
            file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

save_pdf(Vars, TemplateId, Carrier, AccountId, Year, Month) ->
    {'ok', PDF_Data} = create_pdf(Vars, TemplateId, Carrier, AccountId),
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    NewDoc = case kz_datamgr:open_doc(Modb, ?DOC_NAME_FORMAT(Carrier, TemplateId)) of
        {ok, Doc} ->
            kz_json:set_values(Vars, Doc);
        {'error', 'not_found'} ->
            kz_json:set_values(Vars ++ [{<<"_id">>, ?DOC_NAME_FORMAT(Carrier, TemplateId)}
                                       ,{<<"pvt_type">>, ?ONBILL_DOC}
                                       ]
                              ,kz_json:new()) 
    end,
    kz_datamgr:ensure_saved(Modb, NewDoc),
    kz_datamgr:put_attachment(Modb
                             ,?DOC_NAME_FORMAT(Carrier, TemplateId)
                             ,<<(?DOC_NAME_FORMAT(Carrier, TemplateId))/binary, ".pdf">>
                             ,PDF_Data
                             ,[{'content_type', <<"application/pdf">>}]
                            ),
    kz_datamgr:flush_cache_doc(Modb, NewDoc).

-spec generate_docs(ne_binary(), integer(), integer()) -> ok.
generate_docs(AccountId, Year, Month) ->
    Carriers = onbill_util:account_carriers_list(AccountId),
    _ = [generate_docs(AccountId, Year, Month, Carrier) || Carrier <- Carriers],
    maybe_aggregate_invoice(AccountId, Year, Month, Carriers).

generate_docs(AccountId, Year, Month, Carrier) ->
    CarrierDoc = onbill_util:carrier_doc(Carrier, AccountId),
    OnbillResellerVars = onbill_util:reseller_vars(AccountId),
    VatUpdatedFeesList = fees:shape_fees(AccountId, Year, Month, CarrierDoc, OnbillResellerVars),
    Totals = lists:foldl(fun(X, {TN_Acc, VAT_Acc, TB_Acc}) ->
                                                        {TN_Acc + props:get_value(<<"cost_netto">>, X)
                                                         ,VAT_Acc + props:get_value(<<"vat_line_total">>, X)
                                                         ,TB_Acc + props:get_value(<<"cost_brutto">>, X)
                                                        }
                                                      end
                                                      ,{0,0,0}
                                                      ,VatUpdatedFeesList
                                                     ),
    generate_docs(AccountId, Year, Month, Carrier, VatUpdatedFeesList, Totals).

generate_docs(_, _, _, Carrier, _, {TotalNetto, TotalVAT, TotalBrutto})
    when TotalNetto =< 0.0
    orelse TotalVAT =< 0.0
    orelse TotalBrutto =< 0.0
->
    lager:debug("Skipping generate_docs for ~p because of zero usage: TotalNetto: ~p, TotalVAT: ~p, TotalBrutto: ~p"
               ,[Carrier, TotalNetto, TotalVAT, TotalBrutto]);
generate_docs(AccountId, Year, Month, Carrier, VatUpdatedFeesList, {TotalNetto, TotalVAT, TotalBrutto}) ->
    CarrierDoc = onbill_util:carrier_doc(Carrier, AccountId),
    OnbillResellerVars = onbill_util:reseller_vars(AccountId),
    {TotalBruttoDiv, TotalBruttoRem} = total_to_words(TotalBrutto),
    {TotalVatDiv, TotalVatRem} = total_to_words(TotalVAT),
    AccountOnbillDoc = onbill_util:account_vars(AccountId),
    Vars = [{<<"monthly_fees">>, VatUpdatedFeesList}
           ,{<<"account_addr">>, address_to_line(AccountOnbillDoc)}
           ,{<<"total_netto">>, onbill_util:price_round(TotalNetto)}
           ,{<<"total_vat">>, onbill_util:price_round(TotalVAT)}
           ,{<<"total_vat_div">>, TotalVatDiv}
           ,{<<"total_vat_rem">>, TotalVatRem}
           ,{<<"total_brutto">>, onbill_util:price_round(TotalBrutto)}
           ,{<<"total_brutto_div">>, TotalBruttoDiv}
           ,{<<"total_brutto_rem">>, TotalBruttoRem}
           ,{<<"vat_rate">>, kz_json:get_value(<<"vat_rate">>, OnbillResellerVars, 0.0)}
           ,{<<"currency1">>, kz_json:get_value(<<"currency1">>, OnbillResellerVars)}
           ,{<<"agrm_num">>, kz_json:get_value([<<"agrm">>, Carrier, <<"number">>], AccountOnbillDoc)}
           ,{<<"agrm_date">>, kz_json:get_value([<<"agrm">>, Carrier, <<"date">>], AccountOnbillDoc)}
           ,{<<"doc_date">>, ?END_DATE(Month, Year)}
           ,{<<"start_date">>, ?START_DATE(Month, Year)}
           ,{<<"end_date">>, ?END_DATE(Month, Year)}
           ,{<<"carrier_vars">>, kz_json:set_values([{Key, kz_json:get_value(Key, CarrierDoc)}
                                                     || Key <- kz_json:get_keys(CarrierDoc), filter_vars(Key)
                                                    ]
                                                   ,kz_json:new()
                                                   )
            }
           ,{<<"account_vars">>, kz_json:set_values([{Key, kz_json:get_value(Key, AccountOnbillDoc)}
                                                     || Key <- kz_json:get_keys(AccountOnbillDoc), filter_vars(Key)
                                                    ]
                                                   ,kz_json:new()
                                                   )
            }
           ] 
           ++ [{Key, kz_json:get_value(Key, CarrierDoc)} || Key <- kz_json:get_keys(CarrierDoc), filter_vars(Key)]
           ++ [{Key, kz_json:get_value(Key, AccountOnbillDoc)} || Key <- kz_json:get_keys(AccountOnbillDoc), filter_vars(Key)],
    _ = [save_pdf(Vars
                    ++ [{<<"onbill_doc_type">>, DocType}]
                    ++ [{<<"doc_number">>, docs_numbering:get_binary_number(AccountId, Carrier, DocType, Year, Month)}]
                 ,DocType
                 ,Carrier
                 ,AccountId
                 ,Year
                 ,Month
                 )
         || DocType <- kz_json:get_value(<<"onbill_doc_types">>, CarrierDoc)
        ].

total_to_words(Total) ->
    [TotalDiv, TotalRem] = binary:split(float_to_binary(Total,[{decimals,2}]), <<".">>),
    {unicode:characters_to_binary(amount_into_words:render(TotalDiv), unicode, utf8)
    ,unicode:characters_to_binary(amount_into_words:render(TotalRem), unicode, utf8)
    }.

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

maybe_aggregate_invoice(AccountId, Year, Month, Carriers) ->
    AccountOnbillDoc = onbill_util:account_vars(AccountId),
    case kz_json:get_value(<<"aggregate_invoice">>, AccountOnbillDoc) of
        'true' -> aggregate_invoice(AccountId, Year, Month, Carriers);
        _ -> 'ok'
    end.

aggregate_invoice(AccountId, Year, Month, Carriers) ->
    DocType = <<"aggregated_invoice">>,
    OnbillResellerVars = onbill_util:reseller_vars(AccountId),
    MainCarrier = onbill_util:get_main_carrier(Carriers, AccountId),
    MainCarrierDoc = onbill_util:carrier_doc(MainCarrier, AccountId),
    AccountOnbillDoc = onbill_util:account_vars(AccountId),
    {AggregatedVars, TotalNetto, TotalVAT, TotalBrutto} = lists:foldl(fun(Carrier, Acc) ->
                                                                          aggregate_data(AccountId, Year, Month, Carrier, Acc)
                                                                      end
                                                                     ,{[], 0, 0, 0}
                                                                     ,Carriers),
    {TotalBruttoDiv, TotalBruttoRem} = total_to_words(TotalBrutto),
    {TotalVatDiv, TotalVatRem} = total_to_words(TotalVAT),
    Vars = [{<<"aggregated_vars">>, AggregatedVars}
           ,{<<"doc_date">>, ?END_DATE(Month, Year)}
           ,{<<"start_date">>, ?START_DATE(Month, Year)}
           ,{<<"end_date">>, ?END_DATE(Month, Year)}
           ,{<<"total_netto">>, onbill_util:price_round(TotalNetto)}
           ,{<<"total_vat">>, onbill_util:price_round(TotalVAT)}
           ,{<<"total_brutto">>, onbill_util:price_round(TotalBrutto)}
           ,{<<"vat_rate">>, kz_json:get_value(<<"vat_rate">>, OnbillResellerVars, 0.0)}
           ,{<<"total_vat_div">>, TotalVatDiv}
           ,{<<"total_vat_rem">>, TotalVatRem}
           ,{<<"total_brutto_div">>, TotalBruttoDiv}
           ,{<<"total_brutto_rem">>, TotalBruttoRem}
           ,{<<"onbill_doc_type">>, DocType}
           ,{<<"doc_number">>, docs_numbering:get_binary_number(AccountId, MainCarrier, DocType, Year, Month)}
           ]
           ++ [{Key, kz_json:get_value(Key, MainCarrierDoc)} || Key <- kz_json:get_keys(MainCarrierDoc), filter_vars(Key)]
           ++ [{Key, kz_json:get_value(Key, AccountOnbillDoc)} || Key <- kz_json:get_keys(AccountOnbillDoc), filter_vars(Key)],
    save_pdf(Vars, DocType, MainCarrier, AccountId, Year, Month).

aggregate_data(AccountId, Year, Month, Carrier, {AggrVars, TotalNetto, TotalVAT, TotalBrutto}) ->
    TemplateId = <<"invoice">>,
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    case kz_datamgr:open_doc(Modb, ?DOC_NAME_FORMAT(Carrier, TemplateId)) of
        {ok, InvoiceDoc} ->
            {[[{Key, kz_json:get_value(Key, InvoiceDoc)} || Key <- kz_json:get_keys(InvoiceDoc), filter_vars(Key)]] ++ AggrVars
            ,kz_json:get_value(<<"total_netto">>, InvoiceDoc) + TotalNetto
            ,kz_json:get_value(<<"total_vat">>, InvoiceDoc) + TotalVAT
            ,kz_json:get_value(<<"total_brutto">>, InvoiceDoc) + TotalBrutto
            };
        _ -> {AggrVars, TotalNetto, TotalVAT, TotalBrutto} 
    end.
    
-spec per_minute_reports(ne_binary(), integer(), integer()) -> 'ok'.
per_minute_reports(AccountId, Year, Month) ->
    Carriers = onbill_util:account_carriers_list(AccountId),
    _ = [maybe_per_minute_report(AccountId, Year, Month, Carrier) || Carrier <- Carriers].

maybe_per_minute_report(AccountId, Year, Month, Carrier) ->
    {CallsJObjs, CallsTotalSec, CallsTotalSumm} = fees:per_minute_calls(AccountId, Year, Month, Carrier),
    per_minute_report(AccountId, Year, Month, Carrier, CallsJObjs, CallsTotalSec, CallsTotalSumm).

per_minute_report(AccountId, Year, Month, Carrier, CallsJObjs, CallsTotalSec, CallsTotalSumm) when CallsTotalSumm > 0.0 ->
    DocType = <<"calls_report">>,
    OnbillResellerVars = onbill_util:reseller_vars(AccountId),
    CarrierDoc = onbill_util:carrier_doc(Carrier, AccountId),
    AccountOnbillDoc = onbill_util:account_vars(AccountId),
    {CallsJObjs, CallsTotalSec, CallsTotalSumm} = fees:per_minute_calls(AccountId, Year, Month, Carrier),
    Vars = [{<<"per_minute_calls">>, CallsJObjs}
           ,{<<"doc_date">>, ?END_DATE(Month, Year)}
           ,{<<"start_date">>, ?START_DATE(Month, Year)}
           ,{<<"end_date">>, ?END_DATE(Month, Year)}
           ,{<<"agrm_num">>, kz_json:get_value([<<"agrm">>, Carrier, <<"number">>], AccountOnbillDoc)}
           ,{<<"agrm_date">>, kz_json:get_value([<<"agrm">>, Carrier, <<"date">>], AccountOnbillDoc)}
           ,{<<"onbill_doc_type">>, DocType}
           ]
           ++ fees:vatify_amount(<<"total">>, CallsTotalSumm, OnbillResellerVars)
           ++ [{Key, kz_json:get_value(Key, CarrierDoc)} || Key <- kz_json:get_keys(CarrierDoc), filter_vars(Key)]
           ++ [{Key, kz_json:get_value(Key, AccountOnbillDoc)} || Key <- kz_json:get_keys(AccountOnbillDoc), filter_vars(Key)],
    save_pdf(Vars, DocType, Carrier, AccountId, Year, Month);
per_minute_report(_, _, _, _, _, _, _) ->
    'ok'.

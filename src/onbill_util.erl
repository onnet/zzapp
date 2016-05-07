-module(onbill_util).

-export([check_db/1
         ,create_pdf/4
         ,save_pdf/6
         ,maybe_add_design_doc/1
         ,get_attachment/2
         ,monthly_fees/1
         ,days_sequence_reduce/1
         ,generate_docs/3
        ]).

-include("onbill.hrl").

-spec check_db(ne_binary()) -> 'ok'.
check_db(Db) when is_binary(Db) ->
    do_check_db(Db, kz_datamgr:db_exists(Db)).

-spec do_check_db(ne_binary(), boolean()) -> 'ok'.
do_check_db(_Db, 'true') -> 'ok';
do_check_db(Db, 'false') ->
    lager:debug("create Db ~p", [Db]),
    _ = kz_datamgr:db_create(Db).

get_template(TemplateId, Carrier) ->
    case kz_datamgr:fetch_attachment(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier), <<(wh_util:to_binary(TemplateId))/binary, ".tpl">>) of
        {'ok', Template} -> Template;
        {error, not_found} ->
            Template = default_template(TemplateId),
            case kz_datamgr:open_doc(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier)) of
                {'ok', _} ->
                    'ok';
                {'error', 'not_found'} ->
                    NewDoc = wh_json:set_values([{<<"_id">>, ?MOD_CONFIG_TEMLATES(Carrier)}
                                                 ,{<<"carrier_name">>,<<"this_doc_carrier_Name">>}
                                                ]
                                                ,wh_json:new()),
                    kz_datamgr:ensure_saved(?SYSTEM_CONFIG_DB, NewDoc)
            end,
            kz_datamgr:put_attachment(?SYSTEM_CONFIG_DB
                                     ,?MOD_CONFIG_TEMLATES(Carrier)
                                     ,<<(wh_util:to_binary(TemplateId))/binary, ".tpl">>
                                     ,Template
                                     ,[{'content_type', <<"text/html">>}]
                                    ),
            Template
    end.

default_template(TemplateId) ->
    FilePath = <<"applications/onbill/priv/templates/ru/", (wh_util:to_binary(TemplateId))/binary, ".html">>,
    {'ok', Data} = file:read_file(FilePath),
    Data.

get_attachment(AttachmentId, Db) ->
    case kz_datamgr:fetch_attachment(Db, AttachmentId, <<(wh_util:to_binary(AttachmentId))/binary, ".pdf">>) of
        {'ok', _} = OK -> OK;
        E -> E
    end.

prepare_tpl(Vars, TemplateId, Carrier) ->
    ErlyMod = wh_util:to_atom(TemplateId),
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
    Rand = wh_util:rand_hex_binary(5),
    Prefix = <<AccountId/binary, "-", (wh_util:to_binary(TemplateId))/binary, "-", Rand/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),
    HTMLTpl = prepare_tpl(Vars, TemplateId, Carrier),
    file:write_file(HTMLFile, HTMLTpl),
    Cmd = <<(?HTML_TO_PDF(TemplateId))/binary, " ", HTMLFile/binary, " ", PDFFile/binary>>,
    case os:cmd(wh_util:to_list(Cmd)) of
        [] -> file:read_file(PDFFile);
        "\n" -> file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

save_pdf(Vars, TemplateId, Carrier, AccountId, Year, Month) ->
    {'ok', PDF_Data} = create_pdf(Vars, TemplateId, Carrier, AccountId),
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    NewDoc = case kz_datamgr:open_doc(Modb, wh_util:to_binary(TemplateId)) of
        {ok, Doc} -> wh_json:set_values(Vars, Doc);
        {'error', 'not_found'} -> wh_json:set_values(Vars ++ [{<<"_id">>, wh_util:to_binary(TemplateId)}, {<<"pvt_type">>, ?ONBILL_DOC}], wh_json:new()) 
    end,
    kz_datamgr:ensure_saved(Modb, NewDoc),
    kz_datamgr:put_attachment(Modb
                             ,wh_util:to_binary(TemplateId)
                             ,<<(wh_util:to_binary(TemplateId))/binary, ".pdf">>
                             ,PDF_Data
                             ,[{'content_type', <<"application/pdf">>}]
                            ),
    kz_datamgr:flush_cache_doc(Modb, NewDoc).

-spec maybe_add_design_doc(ne_binary()) ->
                                  'ok' |
                                  {'error', 'not_found'}.
maybe_add_design_doc(Modb) ->
    case kz_datamgr:lookup_doc_rev(Modb, <<"_design/onbills">>) of
        {'error', 'not_found'} ->
            lager:warning("adding onbill views to modb: ~s", [Modb]),
            kz_datamgr:revise_doc_from_file(Modb
                                           ,'onbill'
                                           ,<<"views/onbills.json">>
                                          );
        {'ok', _ } -> 'ok'
    end.

generate_docs(AccountId, Year, Month) ->
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    Carrier = <<"onnet">>,
    {'ok', TplDoc} =  kz_datamgr:open_doc(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES(Carrier)),
    {'ok', OnbillCfg} =  kz_datamgr:open_doc(?ONBILL_DB, ?ONBILL_CONFIG),
    {'ok', AccOnbillDoc} =  kz_datamgr:open_doc(?ONBILL_DB, AccountId),
    Docs = ['invoice', 'act'],
    Vars = [{<<"monthly_fees">>, monthly_fees(Modb)}
           ,{<<"account_addr">>, address_to_line(AccOnbillDoc)}
           ,{<<"doc_number">>, <<"13">>}
           ,{<<"vat">>, wh_json:get_value(<<"vat">>, OnbillCfg)}
           ,{<<"agrm_num">>, wh_json:get_value([<<"agrm">>, Carrier, <<"number">>], AccOnbillDoc)}
           ,{<<"agrm_date">>, wh_json:get_value([<<"agrm">>, Carrier, <<"date">>], AccOnbillDoc)}
           ,{<<"doc_date">>, <<"31.",(wh_util:pad_month(Month))/binary,".",(wh_util:to_binary(Year))/binary>>}
           ,{<<"start_date">>, <<"01.",(wh_util:pad_month(Month))/binary,".",(wh_util:to_binary(Year))/binary>>}
           ,{<<"end_date">>, <<"31.03.2016">>}
           ] 
           ++ [{Key, wh_json:get_value(Key, TplDoc)} || Key <- wh_json:get_keys(TplDoc), filter_vars(Key)]
           ++ [{Key, wh_json:get_value(Key, AccOnbillDoc)} || Key <- wh_json:get_keys(AccOnbillDoc), filter_vars(Key)],
    [save_pdf(Vars, TemplateId, Carrier, AccountId, Year, Month) || TemplateId <- Docs].

filter_vars(<<"_", _/binary>>) -> 'false';
filter_vars(<<_/binary>>) -> 'true'.

address_to_line(JObj) ->
    BillingAddrLinnes = wh_json:get_value(<<"billing_address">>, JObj, wh_json:new()),
    {Keys, _} = wh_json:get_values(BillingAddrLinnes),
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
    Modb = wh_util:format_account_modb(Db, 'encoded'),
    RawModb = wh_util:format_account_modb(Db, 'raw'),
    _ = maybe_add_design_doc(Modb),
    RawTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-raw">>, 'latin1'), [duplicate_bag]),
    ResultTableId = ets:new(erlang:binary_to_atom(<<RawModb/binary,"-result">>, 'latin1'), [bag]),
    case kz_datamgr:get_results(Modb, <<"onbills/daily_fees">>, []) of
        {'error', 'not_found'} -> lager:warning("unable to process monthly fee calculaton for Modb: ~s", [Modb]);
        {'ok', JObjs } -> [process_daily_fee(JObj, Modb, RawTableId) || JObj <- JObjs] 
    end,
    _ = process_ets(RawTableId, ResultTableId),
    ServicesList = ets:tab2list(ResultTableId) ++ process_one_time_fees(Db),
    [lager:info("Result Table Line: ~p",[Service]) || Service <- ServicesList],
    services_to_proplist(ServicesList, Year, Month, DaysInMonth).

process_one_time_fees(Modb) ->
    case kz_datamgr:get_results(Modb, <<"onbills/one_time_fees">>, []) of
        {'error', 'not_found'} ->
             lager:warning("no one time charges found in Modb: ~s", [Modb]),
             [];
        {'ok', JObjs } -> [process_one_time_fee(JObj, Modb) || JObj <- JObjs] 
    end.

process_one_time_fee(JObj, Modb) ->
    {'ok', DFDoc} =  kz_datamgr:open_doc(Modb, wh_json:get_value(<<"id">>, JObj)),
    {Year, Month, Day} = wh_util:to_date(wh_json:get_value(<<"pvt_created">>, DFDoc)),
    DaysInMonth = calendar:last_day_of_the_month(Year, Month),
    {wh_json:get_value(<<"pvt_reason">>, DFDoc)
     ,wh_json:get_value(<<"description">>, DFDoc)
     ,wht_util:units_to_dollars(wh_json:get_integer_value(<<"pvt_amount">>, DFDoc))
     ,1
     ,wh_util:to_binary(Day)
     ,DaysInMonth
    }.

process_daily_fee(JObj, Modb, RawTableId) ->
    case kz_datamgr:open_doc(Modb, wh_json:get_value(<<"id">>, JObj)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', DFDoc} -> upload_daily_fee_to_ets(DFDoc, RawTableId)
    end.

upload_daily_fee_to_ets(DFDoc, RawTableId) ->
    ItemsList = wh_json:get_value([<<"pvt_metadata">>, <<"items_history">>],DFDoc),
    [ItemTs|_] = lists:reverse(lists:sort(wh_json:get_keys(ItemsList))),
    Item = wh_json:get_value(ItemTs, ItemsList),
    {{_,_,Day},_} = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(ItemTs)),
    [process_element(wh_json:get_value(ElementKey, Item), RawTableId, Day) || ElementKey <- wh_json:get_keys(Item)
     ,wh_json:is_json_object(wh_json:get_value(ElementKey, Item)) == 'true'
    ].

process_element(Element, RawTableId, Day) ->
    [ets:insert(RawTableId, {category(Unit), item(Unit), rate(Unit), quantity(Unit), Day})
     || {_, Unit} <- wh_json:to_proplist(Element)
     , quantity(Unit) =/= 0.0
    ].

category(Unit) ->
    wh_json:get_value(<<"category">>, Unit).

item(Unit) ->
    wh_json:get_value(<<"item">>, Unit).

rate(Unit) ->
    wh_util:to_float(wh_json:get_value(<<"rate">>, Unit)).

quantity(Unit) ->
    wh_util:to_float(wh_json:get_value(<<"quantity">>, Unit)).

process_ets(RawTableId, ResultTableId) ->
    ServiceTypesList = lists:usort(ets:match(RawTableId,{'$1','_','_','_','_'})),
    [show_items(ServiceType, RawTableId, ResultTableId) || [ServiceType] <- ServiceTypesList].
    
show_items(ServiceType, RawTableId, ResultTableId) ->
    Items = lists:usort(ets:match(RawTableId,{ServiceType,'$2','_','_','_'})),
    [process_ets_item(RawTableId, ResultTableId, ServiceType, Item) || [Item] <- Items].

process_ets_item(RawTableId, ResultTableId, ServiceType, Item) ->
    Prices = lists:usort(ets:match(RawTableId,{ServiceType,Item,'$3','_','_'})),
    [handle_ets_item_price(RawTableId, ResultTableId, ServiceType, Item, Price) || [Price] <- Prices].

handle_ets_item_price(RawTableId, ResultTableId, ServiceType, Item, Price) ->
    Quantities = lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,'$4','_'})),
    [handle_ets_item_quantity(RawTableId, ResultTableId, ServiceType, Item, Price, Quantity) || [Quantity] <- Quantities].

handle_ets_item_quantity(RawTableId, ResultTableId, ServiceType, Item, Price, Quantity) ->
    Days = [Day || [Day] <- lists:usort(ets:match(RawTableId,{ServiceType,Item,Price,Quantity,'$5'}))],
    lager:info("ETS ServiceType: ~p, Item: ~p, Price: ~p, Quantity: ~p, Days: ~p",[ServiceType, Item, Price, Quantity, days_sequence_reduce(Days)]),
    ets:insert(ResultTableId, {ServiceType, Item, Price, Quantity, days_sequence_reduce(Days), length(Days)}).

days_sequence_reduce([Digit]) ->
    days_sequence_reduce([Digit], []);
days_sequence_reduce([First,Last]) ->
    days_sequence_reduce([First,Last], []);
days_sequence_reduce(LongList) ->
    days_glue(days_sequence_reduce(LongList, [])).

days_sequence_reduce([Digit], Acc) ->
    Acc ++ [wh_util:to_binary(Digit)];
days_sequence_reduce([First,Last], Acc) ->
    case First+1 == Last of
        'true' -> Acc ++ [<<(wh_util:to_binary(First))/binary,"-", (wh_util:to_binary(Last))/binary>>];
        'false' -> Acc ++ [<<(wh_util:to_binary(First))/binary,",", (wh_util:to_binary(Last))/binary>>]
    end;
days_sequence_reduce([First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [wh_util:to_binary(First)]);
        'true' -> days_sequence_reduce(First, [Next] ++ T, Acc)
    end.
    
days_sequence_reduce(Prev, [], Acc) ->
    days_sequence_reduce([Prev], Acc);
days_sequence_reduce(Prev, [Digit], Acc) ->
    Acc ++ [<<(wh_util:to_binary(Prev))/binary,"-", (wh_util:to_binary(Digit))/binary>>];
days_sequence_reduce(Prev, [First,Next|T], Acc) ->
    case First+1 == Next of
        'false' -> days_sequence_reduce([Next] ++ T, Acc ++ [<<(wh_util:to_binary(Prev))/binary,"-", (wh_util:to_binary(First))/binary>>]);
        'true' -> days_sequence_reduce(Prev, [Next] ++ T, Acc)
    end.

days_glue(L) ->
    lists:foldl(fun(X,Acc) -> case Acc of <<>> -> X; _ -> <<Acc/binary, ",", X/binary>> end end, <<>>, L).

services_to_proplist(ServicesList, Year, Month, DaysInMonth) ->
    lists:foldl(fun(ServiceLine, Acc) -> service_to_line(ServiceLine, Year, Month, DaysInMonth, Acc) end, [], ServicesList).

service_to_line({ServiceType, Item, Price, Quantity, Period, DaysQty}, Year, Month, DaysInMonth, Acc) ->
    [[{<<"category">>, ServiceType}
    ,{<<"item">>,Item}
    ,{<<"cost">>, DaysQty / DaysInMonth * Price * Quantity}
    ,{<<"rate">>, Price}
    ,{<<"quantity">>, Quantity}
    ,{<<"period">>, Period}
    ,{<<"days_quantity">>, DaysQty}
    ,{<<"days_in_month">>, DaysInMonth}
    ,{<<"month">>, Month}
    ,{<<"month_pad">>, wh_util:pad_month(Month)}
    ,{<<"year">>, Year}
    ]] ++ Acc.

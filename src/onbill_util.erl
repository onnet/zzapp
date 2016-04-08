-module(onbill_util).

-export([check_db/1
         ,create_pdf/3
         ,save_pdf/5
         ,maybe_add_design_doc/1
         ,get_attachment/2
         ,monthly_fee/1
         ,days_sequence_reduce/1
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

get_template(TemplateId) ->
    case kz_datamgr:fetch_attachment(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES, <<(wh_util:to_binary(TemplateId))/binary, ".tpl">>) of
        {'ok', Template} -> Template;
        {error, not_found} ->
            Template = default_template(TemplateId),
            kz_datamgr:put_attachment(?SYSTEM_CONFIG_DB
                                     ,?MOD_CONFIG_TEMLATES
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

prepare_tpl(TemplateId, Vars) ->
    ErlyMod = wh_util:to_atom(TemplateId),
    try erlydtl:compile_template(get_template(TemplateId), ErlyMod, [{'out_dir', 'false'},'return']) of
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

create_pdf(TemplateId, Vars, AccountId) ->
    Rand = wh_util:rand_hex_binary(5),
    Prefix = <<AccountId/binary, "-", (wh_util:to_binary(TemplateId))/binary, "-", Rand/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),
    HTMLTpl = prepare_tpl(TemplateId, Vars),
    file:write_file(HTMLFile, HTMLTpl),
    Cmd = <<(?HTML_TO_PDF(TemplateId))/binary, " ", HTMLFile/binary, " ", PDFFile/binary>>,
    case os:cmd(wh_util:to_list(Cmd)) of
        [] -> file:read_file(PDFFile);
        "\n" -> file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

save_pdf(TemplateId, Vars, AccountId, Year, Month) ->
    {'ok', PDF_Data} = create_pdf(TemplateId, Vars, AccountId),
    Modb = kazoo_modb:get_modb(AccountId, Year, Month),
    kz_datamgr:put_attachment(Modb
                             ,wh_util:to_binary(TemplateId)
                             ,<<(wh_util:to_binary(TemplateId))/binary, ".pdf">>
                             ,PDF_Data
                             ,[{'content_type', <<"application/pdf">>}]
                            ),
    {ok, Doc} = kz_datamgr:open_doc(Modb, wh_util:to_binary(TemplateId)),
    NewDoc = wh_json:set_values(Vars ++ [{<<"pvt_type">>,<<"onbill_doc">>}], Doc),
    kz_datamgr:ensure_saved(Modb, NewDoc).

-spec maybe_add_design_doc(ne_binary()) ->
                                  'ok' |
                                  {'error', 'not_found'}.
maybe_add_design_doc(Db) ->
    case kz_datamgr:lookup_doc_rev(Db, <<"_design/onbills">>) of
        {'error', 'not_found'} ->
            lager:warning("adding onbill views to modb: ~s", [Db]),
            kz_datamgr:revise_doc_from_file(Db
                                           ,'onbill'
                                           ,<<"views/onbills.json">>
                                          );
        {'ok', _ } -> 'ok'
    end.

monthly_fee(Db) ->
    _ = maybe_add_design_doc(Db),
    TableId = ets:new(erlang:binary_to_atom(wh_util:format_account_modb(Db, 'raw'), 'latin1'), [duplicate_bag]),
    case kz_datamgr:get_results(Db, <<"onbills/daily_fees">>) of
        {'error', 'not_found'} -> lager:warning("unable process monthly fee calculaton for Db: ~s", [Db]);
        {'ok', JObjs } -> [process_daily_fee(JObj, Db, TableId) || JObj <- JObjs] 
    end,
    process_ets(TableId).

process_daily_fee(JObj, Db, TableId) ->
    case kz_datamgr:open_doc(Db, wh_json:get_value(<<"id">>, JObj)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', DFDoc} -> upload_daily_fee_to_ets(DFDoc, TableId)
    end.

upload_daily_fee_to_ets(DFDoc, TableId) ->
    ItemsList = wh_json:get_value([<<"pvt_metadata">>, <<"items_history">>],DFDoc),
    [ItemTs|_] = lists:reverse(lists:sort(wh_json:get_keys(ItemsList))),
    Item = wh_json:get_value(ItemTs, ItemsList),
    {{_,_,Day},_} = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(ItemTs)),
    [process_element(wh_json:get_value(ElementKey, Item), TableId, Day) || ElementKey <- wh_json:get_keys(Item)
     ,wh_json:is_json_object(wh_json:get_value(ElementKey, Item)) == 'true'
    ].

process_element(Element, TableId, Day) ->
    [ets:insert(TableId, {category(Unit), item(Unit), rate(Unit), quantity(Unit), Day})
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

process_ets(TableId) ->
    ServiceTypesList = lists:usort(ets:match(TableId,{'$1','_','_','_','_'})),
    [show_items(ServiceType, TableId) || [ServiceType] <- ServiceTypesList].
    
show_items(ServiceType, TableId) ->
    Items = lists:usort(ets:match(TableId,{ServiceType,'$2','_','_','_'})),
    [process_ets_item(TableId, ServiceType, Item) || [Item] <- Items].

process_ets_item(TableId, ServiceType, Item) ->
    Prices = lists:usort(ets:match(TableId,{ServiceType,Item,'$3','_','_'})),
    [handle_ets_item_price(TableId, ServiceType, Item, Price) || [Price] <- Prices].

handle_ets_item_price(TableId, ServiceType, Item, Price) ->
    Quantities = lists:usort(ets:match(TableId,{ServiceType,Item,Price,'$4','_'})),
    [handle_ets_item_quantity(TableId, ServiceType, Item, Price, Quantity) || [Quantity] <- Quantities].

handle_ets_item_quantity(TableId, ServiceType, Item, Price, Quantity) ->
    Days = [Day || [Day] <- lists:usort(ets:match(TableId,{ServiceType,Item,Price,Quantity,'$5'}))],
    lager:info("ETS ServiceType: ~p, Item: ~p, Price: ~p, Quantity: ~p, Days: ~p",[ServiceType, Item, Price, Quantity, days_sequence_reduce(Days)]).

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

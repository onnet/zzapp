-module(onbill_util).

-export([check_db/1
         ,create_pdf/3
         ,save_pdf/5
         ,maybe_add_design_doc/1
         ,get_attachment/2
         ,monthly_fee/1
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
    TableId = ets:new(erlang:binary_to_atom(wh_util:format_account_modb(Db, 'raw'), 'latin1'), [duplicate_bag]),
    case kz_datamgr:get_results(Db, <<"onbills/daily_fees">>) of
        {'error', 'not_found'} -> lager:warning("unable process monthly fee calculaton for Db: ~s", [Db]);
        {'ok', JObjs } -> [process_daily_fee(JObj, Db, TableId) || JObj <- JObjs] 
    end,
    lager:info("ETS Tab: ~p",[ets:tab2list(TableId)]),
    lager:info("ETS phone_numbers: ~p",[ets:match(TableId,{<<"phone_numbers">>,'$2','$3','$4','$5'})]),
    lager:info("ETS monthly_services: ~p",[ets:match(TableId,{<<"monthly_services">>,'$2','$3','$4','$5'})]),
    lager:info("ETS limits: ~p",[ets:match(TableId,{<<"limits">>,'$2','$3','$4','$5'})]).
    

process_daily_fee(JObj, Db, TableId) ->
    case kz_datamgr:open_doc(Db, wh_json:get_value(<<"id">>, JObj)) of
        {'error', 'not_found'} -> 'ok';
        {'ok', DFDoc} -> upload_daily_fee_to_ets(DFDoc, TableId)
    end.

upload_daily_fee_to_ets(DFDoc, TableId) ->
    ItemsList = wh_json:get_value([<<"pvt_metadata">>, <<"items_history">>],DFDoc),
    [ItemTs|_] = lists:reverse(lists:sort(wh_json:get_keys(ItemsList))),
    Item = wh_json:get_value(ItemTs, ItemsList),
    [process_class(wh_json:get_value(ClassKey, Item), TableId, ItemTs) || ClassKey <- wh_json:get_keys(Item)
     ,wh_json:is_json_object(wh_json:get_value(ClassKey, Item)) == 'true'
    ].

process_class(Class, TableId, ItemTs) ->
    lager:info("IAM Class: ~p",[Class]),
    [process_item(Element, TableId, ItemTs) || {_, Element} <- wh_json:to_proplist(Class)].
process_item(Element, TableId, ItemTs) ->
    lager:info("IAM Element: ~p",[Element]),
    ets:insert(TableId, {category(Element), item(Element), rate(Element), quantity(Element), ItemTs}).

category(Element) -> wh_json:get_value(<<"category">>, Element).
item(Element) -> wh_json:get_value(<<"item">>, Element).
rate(Element) -> wh_json:get_value(<<"rate">>, Element).
quantity(Element) -> wh_json:get_value(<<"quantity">>, Element).

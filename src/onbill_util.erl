-module(onbill_util).

-export([check_db/1
         ,create_pdf/3
        ]).

-include("onbill.hrl").

-spec check_db(ne_binary()) -> 'ok'.
check_db(Db) when is_binary(Db) ->
    do_check_db(Db, couch_mgr:db_exists(Db)).

-spec do_check_db(ne_binary(), boolean()) -> 'ok'.
do_check_db(_Db, 'true') -> 'ok';
do_check_db(Db, 'false') ->
    lager:debug("create Db ~p", [Db]),
    _ = couch_mgr:db_create(Db).

get_template(TemplateId) ->
    case couch_mgr:fetch_attachment(?SYSTEM_CONFIG_DB, ?MOD_CONFIG_TEMLATES, <<(wh_util:to_binary(TemplateId))/binary, ".tpl">>) of
        {'ok', Template} -> Template;
        {error, not_found} ->
            couch_mgr:put_attachment(?SYSTEM_CONFIG_DB
                                     ,?MOD_CONFIG_TEMLATES
                                     ,<<(wh_util:to_binary(TemplateId))/binary, ".tpl">>
                                     ,?DEFAULT_TEMPLATE(TemplateId)
                                     ,[{'content_type', <<"text/html">>}]
                                    ),
            ?DEFAULT_TEMPLATE(TemplateId)
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
    Cmd = <<(?HTML_TO_PDF)/binary, " ", HTMLFile/binary, " ", PDFFile/binary>>,
    case os:cmd(wh_util:to_list(Cmd)) of
        "\n" -> file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.




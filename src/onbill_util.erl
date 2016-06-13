-module(onbill_util).

-export([check_db/1
         ,maybe_add_design_doc/1
         ,get_attachment/2
         ,price_round/1
         ,account_carriers_list/1
         ,account_doc/1
         ,carrier_doc/1
         ,reseller_vars/1
         ,maybe_main_carrier/1
         ,get_main_carrier/1
         ,format_datetime/1
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

-spec maybe_add_design_doc(ne_binary()) -> 'ok' | {'error', 'not_found'}.
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

get_attachment(AttachmentId, Db) ->
    case kz_datamgr:fetch_attachment(Db, AttachmentId, <<(kz_util:to_binary(AttachmentId))/binary, ".pdf">>) of
        {'ok', _} = OK -> OK;
        E -> E
    end.

price_round(Price) ->
    round(Price * 100) / 100.

account_carriers_list(AccountId) ->
    kz_json:get_value(<<"carriers">>, reseller_vars(AccountId), []).

account_doc(AccountId) ->
    {'ok', AccountDoc} =  kz_datamgr:open_doc(?ONBILL_DB, AccountId),
    AccountDoc.

carrier_doc(Carrier) ->
    {'ok', CarrierDoc} =  kz_datamgr:open_doc(?ONBILL_DB, ?CARRIER_DOC(Carrier)),
    CarrierDoc.

reseller_vars(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    {'ok', GlobalVars} =  kz_datamgr:open_doc(?ONBILL_DB, ResellerId),
    GlobalVars.

maybe_main_carrier(Carrier) when is_binary(Carrier) ->
    maybe_main_carrier(carrier_doc(Carrier));
maybe_main_carrier(CarrierDoc) ->
    case kz_json:get_value(<<"carrier_type">>, CarrierDoc) of
        <<"main">> -> 'true';
        _ -> 'false'
    end.

get_main_carrier([Carrier]) ->
    Carrier;
get_main_carrier([Carrier|T]) ->
    case maybe_main_carrier(Carrier) of
        'true' -> Carrier;
        _ -> get_main_carrier(T)
    end;
get_main_carrier(AccountId) ->
    get_main_carrier(account_carriers_list(AccountId)).

format_datetime(TStamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(TStamp),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    kz_util:to_binary(StrTime).

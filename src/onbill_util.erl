-module(onbill_util).

-export([check_db/1
         ,maybe_add_design_doc/2
         ,get_attachment/2
         ,price_round/1
         ,account_carriers_list/1
         ,account_doc/1
         ,carrier_doc/1
         ,reseller_vars/1
         ,maybe_main_carrier/1
         ,get_main_carrier/1
         ,format_datetime/1
         ,is_billable/1
         ,validate_relationship/2
         ,normalize_view_results/2
         ,normalize_view_active_results/2
         ,maybe_fee_active/2
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

-spec maybe_add_design_doc(ne_binary(), ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_design_doc(DbName, ViewName) ->
    case kz_datamgr:lookup_doc_rev(DbName, <<"_design/", ViewName/binary>>) of
        {'error', 'not_found'} ->
            lager:warning("adding onbill views to modb: ~s", [DbName]),
            kz_datamgr:revise_doc_from_file(DbName
                                           ,'onbill'
                                           ,<<"views/", ViewName/binary, ".json">>
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

is_billable(AccountId) ->
    case kz_datamgr:open_doc(?ONBILL_DB, AccountId) of
        {'ok', _} -> 'true';
        _ -> 'false'
    end.

-spec validate_relationship(ne_binary(), ne_binary()) -> boolean().
validate_relationship(ChildId, ResellerId) ->
    case get_children_list(ResellerId) of
        {'ok', Accounts} ->
            AccountIds = lists:map(fun(Account) -> kz_json:get_value(<<"id">>, Account) end, Accounts),
            lists:member(ChildId, AccountIds);
        {'error', _Reason} = E ->
            lager:info("failed to load children. error: ~p", [E]),
            'false'
    end.

get_children_list(ResellerId) ->
    ViewOpts = [{'startkey', [ResellerId]}
               ,{'endkey', [ResellerId, kz_json:new()]}
               ],
    kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_view_active_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_active_results(JObj, Acc) ->
    case maybe_fee_active(kz_util:current_tstamp(), JObj) of
        'true' ->
            [kz_json:get_value(<<"value">>, JObj)|Acc];
        'false' ->
            Acc
    end.

maybe_fee_active(LookupTstamp, Fee) ->
    LookupTstamp > kz_json:get_value([<<"value">>, <<"service_starts">>], Fee)
    andalso
    LookupTstamp < kz_json:get_value([<<"value">>, <<"service_ends">>], Fee).


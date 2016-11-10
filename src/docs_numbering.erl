-module(docs_number).

-export([get_number/5
        ,number_lookup/3
        ]).

-include("onbill.hrl").

-define(RECENT_NUMBER_VIEW, <<"doc_numbering/recent_number">>).
-define(NUMBER_LOOKUP_VIEW, <<"doc_numbering/number_lookup">>).

get_number(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    case number_lookup(AccountId, Carrier, DocType, Year, Month) of
        {'ok', Number} -> {'ok', Number};
        {'error', 'mot_found'} -> maybe_get_new_number(AccountId, Carrier, DocType, Year, Month),
        E -> E
    end.

number_lookup(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    Opts = [{'startkey', [Carrier, DocType, Month, AccountId]}
           ,{'endkey', [Carrier, DocType, Month, AccountId]}]
    case kz_datamgr:get_results(DbName, ?NUMBER_LOOKUP_VIEW, Opts) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} -> {'ok', kz_json:get_integer_value(<<"value">>,JObj)};
        E -> {'error', E}
    end.

get_recent_number(AccountId, Carrier, DocType, Year, _Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    Opts = [{'startkey', [<<"beeline">>,<<"invoice">>, 9]}
           ,{'endkey', [<<"beeline">>,<<"invoice">>, 0]}
           ,'descending'
           ,{limit,1}
           ],
    case kz_datamgr:get_results(DbName, ?RECENT_NUMBER_VIEW, Opts) of
        {'ok', []} -> {'ok', 0};
        {'ok', [JObj]} -> {'ok', kz_json:get_integer_value(<<"value">>,JObj)};
        E -> {'error', E}
    end.

get_new_number(AccountId, Carrier, DocType, Year, Month) ->
    case get_recent_number(AccountId, Carrier, DocType, Year, _Month) of
        {'ok', RecentNumber} -> {'ok', RecentNumber + 1};
        {'error', E} -> {'error', E}
    end.

-module(docs_number).

-export([get_number/5
    %    ,lookup_number/3
        ]).

-include("onbill.hrl").

get_number(AccountId, _Carrier, _DocType, Year, _Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    <<"docs_number_13">>.

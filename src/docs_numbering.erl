-module(docs_numbering).

-export([get_number/5
        ,number_lookup/5
        ]).

-include("onbill.hrl").

-define(VIEW_NAME, <<"docs_numbering">>).
-define(RECENT_NUMBER_VIEW, <<(?VIEW_NAME)/binary, "/recent_number">>).
-define(NUMBER_LOOKUP_VIEW, <<(?VIEW_NAME)/binary, "/number_lookup">>).
-define(DOCS_LOOKUP_VIEW, <<(?VIEW_NAME)/binary, "/docs_lookup">>).

get_number(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
    case number_lookup(AccountId, Carrier, DocType, Year, Month) of
        {'ok', Number} -> {'ok', Number};
        {'error', 'not_found'} -> maybe_get_new_number(AccountId, Carrier, DocType, Year, Month);
        E -> E
    end.

number_lookup(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
    Opts = [{'startkey', [Carrier, DocType, Month, AccountId]}
           ,{'endkey', [Carrier, DocType, Month, AccountId]}],
    case kz_datamgr:get_results(DbName, ?NUMBER_LOOKUP_VIEW, Opts) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} -> {'ok', kz_json:get_integer_value(<<"value">>,JObj)};
        E -> E
    end.

%%
%% We can issue new number for requested Month in case there is no such
%% type of documents generated in later periods
%%
maybe_get_new_number(AccountId, Carrier, DocType, Year, Month) ->
    {NextMonthYear,NextMonth} = onbill_util:next_month(Year, Month),
    case no_docs_in_year_since_month(AccountId, Carrier, DocType, NextMonthYear, NextMonth)
           andalso no_docs_in_year_since_month(AccountId, Carrier, DocType, NextMonthYear+1, 1)
    of
        'true' -> get_new_number(AccountId, Carrier, DocType, Year, Month);
        'false' -> {'error', 'period_closed'}
    end.

no_docs_in_year_since_month(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
    Opts = [{'startkey', [Carrier, DocType, Month]}
           ,{'endkey', [Carrier, DocType, 12]}],
    case kz_datamgr:get_results(DbName, ?DOCS_LOOKUP_VIEW, Opts) of
        {'ok', []} -> 'true';
        {'ok', _} -> 'false';
        E -> E
    end.

get_new_number(AccountId, Carrier, DocType, Year, Month) ->
    case get_recent_number(AccountId, Carrier, DocType, Year, Month) of
        {'ok', RecentNumber} -> {'ok', RecentNumber + 1};
        E -> E
    end.

%%
%% Create options for numbering:
%% - start from 1 every beginning of the year;
%% - couninious numbering (if all existing documents should be numbered sequentially throughout)
%%

get_recent_number(AccountId, Carrier, DocType, Year, _Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOC_NUMBER_DOC(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    Opts = [{'startkey', [Carrier,DocType, 999999]}
           ,{'endkey', [Carrier,DocType, 0]}
           ,'descending'
           ,{limit,1}
           ],
    case kz_datamgr:get_results(DbName, ?RECENT_NUMBER_VIEW, Opts) of
        {'ok', []} -> {'ok', 0};
        {'ok', [JObj]} -> {'ok', kz_json:get_integer_value(<<"value">>,JObj)};
        E -> E
    end.

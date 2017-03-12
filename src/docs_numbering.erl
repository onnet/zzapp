-module(docs_numbering).

-export([get_binary_number/5
        ,get_new_binary_number/3
        ,number_lookup/5
        ]).

-include("onbill.hrl").

-define(VIEW_NAME, <<"docs_numbering">>).
-define(RECENT_NUMBER_VIEW, <<(?VIEW_NAME)/binary, "/recent_number">>).
-define(NUMBER_LOOKUP_VIEW, <<(?VIEW_NAME)/binary, "/number_lookup">>).
-define(DOCS_LOOKUP_VIEW, <<(?VIEW_NAME)/binary, "/docs_lookup">>).
-define(NUMBER_DOC_ID(Carrier, DocType, DocNumber), <<(?TO_BIN(Carrier))/binary,".",(?TO_BIN(DocType))/binary,".", (?TO_BIN(DocNumber))/binary>>).


-spec get_binary_number(ne_binary(), ne_binary(), ne_binary(), integer(), integer()) -> ne_binary().
get_binary_number(AccountId, Carrier, DocType, Year, Month) ->
    case get_number(AccountId, Carrier, DocType, Year, Month) of
        {'ok', Number} -> ?TO_BIN(Number);
        _E -> <<"ERROR WRONG NUMBER">>
    end.

-spec get_new_binary_number(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
get_new_binary_number(AccountId, Carrier, DocType) ->
    {Year, Month, _} = erlang:date(),
    case get_new_number(AccountId, Carrier, DocType, Year, Month) of
        {'ok', Number} -> ?TO_BIN(Number);
        _E -> <<"ERROR WRONG NUMBER">>
    end.

get_number(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOCS_NUMBER_DB(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
    case number_lookup(AccountId, Carrier, DocType, Year, Month) of
        {'ok', Number} -> {'ok', Number};
        {'error', 'not_found'} -> maybe_get_new_number(AccountId, Carrier, DocType, Year, Month);
        E -> E
    end.

-spec number_lookup(ne_binary(), ne_binary(), ne_binary(), integer(), integer()) -> {'ok', integer()}|{'error', atom()}.
number_lookup(AccountId, Carrier, DocType, Year, Month) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOCS_NUMBER_DB(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
    Opts = [{'startkey', [Carrier, DocType, Month, AccountId]}
           ,{'endkey', [Carrier, DocType, Month, AccountId]}
           ],
    case kz_datamgr:get_results(DbName, ?NUMBER_LOOKUP_VIEW, Opts) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', ResultList} ->
            {'ok', lists:max([kz_json:get_value(<<"value">>, R) || R <- ResultList])};
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
    DbName = ?DOCS_NUMBER_DB(ResellerId, Year),
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
    case get_recent_number(AccountId, Carrier, DocType, Year) of
        {'ok', RecentNumber} -> reserve_number(AccountId, Carrier, DocType, Year, Month, RecentNumber + 1, 1);
        E -> E
    end.

reserve_number(AccountId, Carrier, DocType, Year, Month, NumberToReserve, Attempt) when Attempt < 3 ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOCS_NUMBER_DB(ResellerId, Year),
    Values = [{<<"_id">>, ?NUMBER_DOC_ID(Carrier, DocType, NumberToReserve)}
             ,{<<"carrier">>, Carrier}
             ,{<<"account_id">>, AccountId}
             ,{<<"month_assigned">>, Month}
             ,{<<"pvt_type">>, <<"onbill_doc">>}
             ,{<<"onbill_doc_type">>, DocType}
             ,{<<"number_assigned">>, NumberToReserve}
             ],
    NewDoc = kz_json:set_values(Values, kz_json:new()),
    case kz_datamgr:save_doc(DbName, NewDoc) of
     %   {'ok', _} -> get_number(AccountId, Carrier, DocType, Year, Month);
        {'ok', _} -> {'ok', NumberToReserve};
        {error,conflict} -> reserve_number(AccountId, Carrier, DocType, Year, Month, NumberToReserve, Attempt+1);
        E -> E
    end.

%%
%% Create options for numbering:
%% - start from 1 every beginning of the year;
%% - couninious numbering (if all existing documents should be numbered sequentially throughout)
%%

get_recent_number(AccountId, Carrier, DocType, Year) ->
    case get_year_recent_number(AccountId, Carrier, DocType, Year) of
        {'ok', 0} -> maybe_continious_numbering(AccountId, Carrier, DocType, Year);
        {'ok', RecentDocNum} -> {'ok', RecentDocNum};
        E -> E
    end.

get_year_recent_number(AccountId, Carrier, DocType, Year) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?DOCS_NUMBER_DB(ResellerId, Year),
    _ = onbill_util:check_db(DbName),
    onbill_util:maybe_add_design_doc(DbName, ?VIEW_NAME),
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

maybe_continious_numbering(AccountId, Carrier, DocType, Year) ->
    case kz_json:is_true(<<"continious_doc_numbering">>, onbill_util:carrier_doc(Carrier, AccountId)) of
        'true' ->
            ResellerId = kz_services:find_reseller_id(AccountId),
            case kz_datamgr:db_exists(?DOCS_NUMBER_DB(ResellerId, Year-1)) of
                'true' ->
                    get_year_recent_number(AccountId, Carrier, DocType, Year-1);
                'false' ->
                    {'ok', 0}
            end;
        'false' ->
            {'ok', 0}
    end.

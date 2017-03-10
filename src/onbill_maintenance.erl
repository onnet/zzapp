-module(onbill_maintenance).

-export([populate_modb_day_with_fee/4
         ,populate_modb_with_fees/3
         ,refresh/0
        ]).

-include("onbill.hrl").

-spec populate_modb_with_fees(ne_binary(), integer(), integer()) -> ok.
populate_modb_with_fees(AccountId, Year, Month) ->
    kz_bookkeeper_onbill:populate_modb_with_fees(kz_term:to_binary(AccountId), Year, Month).

-spec populate_modb_day_with_fee(ne_binary(), integer(), integer(), integer()) -> ok.
populate_modb_day_with_fee(AccountId, Year, Month, Day) ->
    kz_bookkeeper_onbill:populate_modb_day_with_fee(kz_term:to_binary(AccountId), Year, Month, Day).

-spec refresh() -> 'no_return'.
-spec refresh(ne_binary() | nonempty_string()) -> 'ok' | 'remove'.
-spec refresh(ne_binaries(), text() | non_neg_integer()) -> 'no_return'.
-spec refresh(ne_binaries(), non_neg_integer(), non_neg_integer()) -> 'no_return'.
refresh() ->
    Databases = get_databases(),
    refresh(Databases, 2 * ?MILLISECONDS_IN_SECOND).

refresh(Databases, Pause) ->
    Total = length(Databases),
    refresh(Databases, kz_term:to_integer(Pause), Total).

refresh([], _, _) -> 'no_return';
refresh([Database|Databases], Pause, Total) ->
    io:format("(~p/~p) refreshing database '~s'~n"
             ,[length(Databases) + 1, Total, Database]),
    _ = refresh(Database),
    _ = case Pause < 1 of
            'false' -> timer:sleep(Pause);
            'true' -> 'ok'
        end,
    refresh(Databases, Pause, Total).

-spec get_databases() -> ne_binaries().
get_databases() ->
        {'ok', Databases} = kz_datamgr:db_info(),
            lists:sort(fun get_database_sort/2, lists:usort(Databases ++ ?KZ_SYSTEM_DBS)).

-spec get_database_sort(ne_binary(), ne_binary()) -> boolean().
get_database_sort(Db1, Db2) ->
        kzs_util:db_priority(Db1) < kzs_util:db_priority(Db2).

refresh(<<"onbill-", _/binary>> = DbName) ->
    kz_datamgr:db_create(DbName),
    _ = kz_datamgr:revise_doc_from_file(DbName, 'onbill', <<"views/docs_numbering.json">>),
    'ok';
refresh(DbName) when is_binary(DbName) ->
    case kz_datamgr:db_classification(DbName) of
        'account' ->
            _ = kz_datamgr:revise_doc_from_file(DbName, 'onbill', <<"views/onbill_e911.json">>),
            _ = kz_datamgr:revise_doc_from_file(DbName, 'onbill', <<"views/periodic_fees.json">>);
        'modb' ->
            _ = kz_datamgr:revise_doc_from_file(DbName, 'onbill', <<"views/onbills.json">>);
        _Else -> 'ok'
    end.


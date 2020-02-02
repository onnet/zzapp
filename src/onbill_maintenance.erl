-module(onbill_maintenance).

-export([populate_modb_day_with_fee/4
        ,populate_modb_with_fees/3
        ,refresh/0
%        ,correct_billing_id/0
        ,set_billing_day/1
        ,set_billing_day/2
        ,set_rate_value/5
        ,set_rate_list_value/5
        ]).

-include("onbill.hrl").

-spec populate_modb_with_fees(kz_term:ne_binary(), integer(), integer()) -> ok.
populate_modb_with_fees(AccountId, Year, Month) ->
    kz_bookkeeper_onbill:populate_modb_with_fees(kz_term:to_binary(AccountId), ?TO_INT(Year), ?TO_INT(Month)).

-spec populate_modb_day_with_fee(kz_term:ne_binary(), integer(), integer(), integer()) -> ok.
populate_modb_day_with_fee(AccountId, Year, Month, Day) ->
    kz_bookkeeper_onbill:populate_modb_day_with_fee(kz_term:to_binary(AccountId), ?TO_INT(Year), ?TO_INT(Month), ?TO_INT(Day)).

-spec refresh() -> 'no_return'.
-spec refresh(kz_term:ne_binaries(), non_neg_integer()) -> 'no_return'.
-spec refresh(kz_term:ne_binary(), non_neg_integer(), non_neg_integer()) -> 'ok'.
refresh() ->
    kz_datamgr:revise_docs_from_folder(<<"system_schemas">>, 'onbill', "schemas"),
    Databases = get_databases(),
    refresh(Databases, length(Databases) + 1).

refresh(<<"onbill-", _/binary>> = DbName, DbLeft, Total) ->
    io:format("(~p/~p) refreshing database '~s'~n",[DbLeft, Total, DbName]),
    _ = kz_datamgr:revise_doc_from_file(DbName, 'onbill', <<"views/docs_numbering.json">>),
    timer:sleep(?PAUSE);
refresh(DbName, DbLeft, Total) when is_binary(DbName) ->
    case kz_datamgr:db_classification(DbName) of
        'account' ->
            AccountDb = kz_util:format_account_id(DbName, 'encoded'),
            io:format("(~p/~p) refreshing database '~s'~n",[DbLeft, Total, AccountDb]),
            _ = kz_datamgr:revise_doc_from_file(AccountDb, 'onbill', <<"views/onbill_e911.json">>),
            _ = kz_datamgr:revise_doc_from_file(AccountDb, 'onbill', <<"views/periodic_fees.json">>),
            timer:sleep(?PAUSE);
        'modb' ->
            Modb = kz_util:format_account_modb(DbName, 'encoded'),
            io:format("(~p/~p) refreshing database '~s'~n",[DbLeft, Total, Modb]),
            _ = kz_datamgr:revise_doc_from_file(Modb, 'onbill', <<"views/onbills.json">>),
            timer:sleep(?PAUSE);
        _Else ->
            io:format("(~p/~p) skipping database '~s'~n",[DbLeft, Total, DbName]),
            'ok'
    end.

refresh([], _) -> 'no_return';
refresh([Database|Databases], Total) ->
    _ = refresh(Database, length(Databases) + 1, Total),
    refresh(Databases, Total).

-spec get_databases() -> kz_term:ne_binaries().
get_databases() ->
        {'ok', Databases} = kz_datamgr:db_info(),
            lists:sort(fun get_database_sort/2, lists:usort(Databases)).

-spec get_database_sort(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
get_database_sort(Db1, Db2) ->
        kzs_util:db_priority(Db1) < kzs_util:db_priority(Db2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%u%%%%  Manipulate account billing_id handling %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Looks like deprecated in 4.3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%-spec correct_billing_id() -> 'no_return'.
%-spec correct_billing_id(kz_term:ne_binaries(), non_neg_integer()) -> 'no_return'.
%correct_billing_id() ->
%    Databases = get_databases(),
%    correct_billing_id(Databases, length(Databases) + 1).
%
%correct_billing_id([], _) -> 'no_return';
%correct_billing_id([Database|Databases], Total) ->
%    case kz_datamgr:db_classification(Database) of
%        'account' ->
%            AccountId = kz_util:format_account_id(Database, 'raw'),
%            EncodedDb = kz_util:format_account_id(Database, 'encoded'),
%            zz_util:process_documents([<<"billing_id">>], [], EncodedDb, [AccountId]),
%            zz_util:process_documents([<<"billing_id">>], [], <<"accounts">>, [AccountId]),
%            case kz_services:set_billing_id(AccountId, AccountId) of
%                'undefined' ->
%                    io:format("(~p/~p) skipping account database '~s' (~p) ~n",[length(Databases) + 1, Total, Database, AccountId]);
%                Services ->
%                    io:format("(~p/~p) updating account database '~s' (~p) ~n",[length(Databases) + 1, Total, Database, AccountId]),
%                    kz_datamgr:save_doc(<<"services">>, kz_services:to_json(Services)),
%                    timer:sleep(?PAUSE)
%            end;
%        _Else ->
%            io:format("(~p/~p) skipping database '~s'~n",[length(Databases) + 1, Total, Database]),
%            'ok'
%    end,
%    correct_billing_id(Databases, Total).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%  Set account billing day %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set_billing_day(kz_term:ne_binary()) -> 'ok'.
set_billing_day(Day) ->
    Databases = get_databases(),
    set_billing_day(Day, Databases, length(Databases) + 1).

-spec set_billing_day(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_billing_day(AccountId, Day) ->
    zz_util:set_billing_day(?TO_INT(Day), AccountId),
    'ok'.

-spec set_billing_day(kz_term:ne_binary(), kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
set_billing_day(_, [], _) -> 'no_return';
set_billing_day(Day, [Database|Databases], Total) ->
    case kz_datamgr:db_classification(Database) of
        'account' ->
            AccountId = kz_util:format_account_id(Database, 'raw'),
            io:format("(~p/~p) updating account database '~s' (~p) ~n",[length(Databases) + 1, Total, Database, AccountId]),
            zz_util:set_billing_day(?TO_INT(Day), AccountId),
            timer:sleep(?PAUSE);
        _Else ->
            io:format("(~p/~p) skipping database '~s'~n",[length(Databases) + 1, Total, Database]),
            'ok'
    end,
    set_billing_day(Day, Databases, Total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%  Set option to ratedeck  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set_rate_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_rate_value(RatedeckDb, SetKey, SetValue, LookupKey, LookupValue) ->
    case kz_datamgr:get_result_ids(RatedeckDb, <<"rates/crossbar_listing">>) of
        {ok,DocIds} ->
            zz_util:process_documents_case([], [{SetKey, SetValue}], RatedeckDb, DocIds, {LookupKey, LookupValue});
        _ ->
            io:format("No rates found ~n"),
            'ok'
    end.

-spec set_rate_list_value(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_rate_list_value(RatedeckDb, SetKey, SetCommaSeparatedValues, LookupKey, LookupValue) ->
    case kz_datamgr:get_result_ids(RatedeckDb, <<"rates/crossbar_listing">>) of
        {ok,DocIds} ->
            Opts = binary:split(SetCommaSeparatedValues, <<",">>),
            zz_util:process_documents_case([], [{SetKey, Opts}], RatedeckDb, DocIds, {LookupKey, LookupValue});
        _ ->
            io:format("No rates found ~n"),
            'ok'
    end.


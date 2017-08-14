-module(onnet_maintenance).

-export([cleanup_account_doc/0
        ,cleanup_user_doc/0
        ,cleanup_device_doc/0
        ,cleanup_trunkstore_doc/0
        ]).

-include("onbill.hrl").

-define(PAUSE, 300).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  CleanUp documents   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cleanup_account_doc() -> 'no_return'.
cleanup_account_doc() ->
    Databases = get_databases(),
    DelKeys =
        [
        ],
    SetValues =
        [
        ],
    cleanup_doc('account', <<"account/listing_by_name">>, DelKeys, SetValues, Databases, length(Databases) + 1).

-spec cleanup_user_doc() -> 'no_return'.
cleanup_user_doc() ->
    Databases = get_databases(),
    DelKeys =
        [[<<"media">>,<<"fax">>]
        ,[<<"media">>,<<"peer_to_peer">>]
        ],
    SetValues =
         [{[<<"media">>,<<"fax_option">>], 'true'}
         ,{[<<"media">>,<<"audio">>,<<"codecs">>], [<<"PCMA">>,<<"PCMU">>]}
         ],
    cleanup_doc('account', <<"users/crossbar_listing">>, DelKeys, SetValues, Databases, length(Databases) + 1).

-spec cleanup_device_doc() -> 'no_return'.
cleanup_device_doc() ->
    Databases = get_databases(),
    DelKeys =
        [[<<"media">>,<<"fax">>]
        ,[<<"media">>,<<"peer_to_peer">>]
        ],
    SetValues =
         [{[<<"media">>,<<"fax_option">>], 'true'}
         ,{[<<"media">>,<<"audio">>,<<"codecs">>], [<<"PCMA">>,<<"PCMU">>]}
         ],
    cleanup_doc('account', <<"devices/crossbar_listing">>, DelKeys, SetValues, Databases, length(Databases) + 1).

-spec cleanup_doc(atom(), ne_binary(), kz_proplist(), kz_proplist(),  ne_binaries(), non_neg_integer()) -> 'no_return'.
cleanup_doc(_, _, _, _, [], _) -> 'no_return';
cleanup_doc(DbType, View, DelKeys, SetValues, [Database|Databases], Total) ->
    case kz_datamgr:db_classification(Database) of
        DbType ->
            EncodedDb = kz_util:format_account_id(Database, 'encoded'),
            case kz_datamgr:get_result_ids(EncodedDb, View) of
                {ok,DocIds} ->
                    process_document(DelKeys, SetValues, EncodedDb, DocIds);
                _ ->
                    io:format("(~p/~p) no documents of interest in database '~s'~n",[length(Databases) + 1, Total, Database]),
                    'ok'
            end;
        _Else ->
            io:format("(~p/~p) skipping database '~s'~n",[length(Databases) + 1, Total, Database]),
            'ok'
    end,
    cleanup_doc(DbType, View, DelKeys, SetValues, Databases, Total).

process_document(_, _, _, []) ->
    'ok';
process_document(DelKeys, SetValues, EncodedDb, [DeviceId|T]) ->
    io:format("found doc ~p in database '~s'~n",[DeviceId, EncodedDb]),
    {'ok', Doc} = kz_datamgr:open_doc(EncodedDb, DeviceId),
    TmpDoc = kz_json:delete_keys(DelKeys, Doc),
    NewDoc = kz_json:set_values(SetValues, TmpDoc),
    kz_datamgr:save_doc(EncodedDb, NewDoc),
    timer:sleep(?PAUSE),
    process_document(DelKeys, SetValues, EncodedDb, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%  Manipulate trunkstore media handling %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cleanup_trunkstore_doc() -> 'no_return'.
-spec cleanup_trunkstore_doc(ne_binaries(), non_neg_integer()) -> 'no_return'.
cleanup_trunkstore_doc() ->
    Databases = get_databases(),
    cleanup_trunkstore_doc(Databases, length(Databases) + 1).

cleanup_trunkstore_doc([], _) -> 'no_return';
cleanup_trunkstore_doc([Database|Databases], Total) ->
    case kz_datamgr:db_classification(Database) of
        'account' ->
            AccountDb = kz_util:format_account_id(Database, 'encoded'),
            case kz_datamgr:get_result_ids(AccountDb, <<"trunkstore/lookup_user_flags">>) of
                {ok,[DocId|_]} ->
                    io:format("(~p/~p) found trunkstore doc in database '~s'~n",[length(Databases) + 1, Total, Database]),
                    {'ok', TsDoc} = kz_datamgr:open_doc(AccountDb, DocId),
                    [Server|H] = kz_json:get_value(<<"servers">>,TsDoc),
                    Values =
                        [{[<<"options">>,<<"media_handling">>], <<"process">>}
                        ,{[<<"options">>,<<"inbound_format">>], <<"e164">>}
                        ],
                    NewServer = kz_json:set_values(Values, Server),
                    TsDocNew = kz_json:set_value(<<"servers">>, [NewServer|H], TsDoc),
                    kz_datamgr:save_doc(AccountDb,TsDocNew),
                    timer:sleep(?PAUSE);
                _ ->
                    io:format("(~p/~p) no trunkstore doc in database '~s'~n",[length(Databases) + 1, Total, Database]),
                    'ok'
            end;
        _Else ->
            io:format("(~p/~p) skipping database '~s'~n",[length(Databases) + 1, Total, Database]),
            'ok'
    end,
    cleanup_trunkstore_doc(Databases, Total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%  Some util %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_databases() -> ne_binaries().
get_databases() ->
        {'ok', Databases} = kz_datamgr:db_info(),
            lists:sort(fun get_database_sort/2, lists:usort(Databases)).

-spec get_database_sort(ne_binary(), ne_binary()) -> boolean().
get_database_sort(Db1, Db2) ->
        kzs_util:db_priority(Db1) < kzs_util:db_priority(Db2).

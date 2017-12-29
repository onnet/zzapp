-module(onbill_lb).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_mysql_child/0
        ,lbuid_by_uuid/1
        ,account_balance/1
        ,get_main_agrm_id/1
        ,add_payment/2
        ,sync_onbill_lb_info/2
        ]).

-include_lib("onbill.hrl").

-define(LB_MYSQL_POOL, 'lb_mysql').

-spec maybe_mysql_child() -> kz_proplists().
maybe_mysql_child() ->
    case kapps_config:get_is_true(<<"onbill">>, <<"mysql_pool_enable">>, 'false') of
        'true' ->
            PoolOptions  = [{size, 10}, {max_overflow, 20}],
            MySqlOptions = [{host, kapps_config:get_string(<<"onbill">>, <<"mysql_host">>, <<"localhost">>)}
                           ,{user, kapps_config:get_string(<<"onbill">>, <<"mysql_user">>, <<"user">>)}
                           ,{password, kapps_config:get_string(<<"onbill">>, <<"mysql_password">>, <<"password">>)}
                           ,{database, kapps_config:get_string(<<"onbill">>, <<"mysql_database">>, <<"database">>)}
                           ],
            [mysql_poolboy:child_spec(?LB_MYSQL_POOL, PoolOptions, MySqlOptions)];
        'false' ->
            []
    end.

-spec lbuid_by_uuid(ne_binary()) -> any().
lbuid_by_uuid(AccountId) ->
    case mysql_poolboy:query(?LB_MYSQL_POOL
                            ,<<"select uid from accounts where uuid = ? limit 1">>
                            ,[AccountId])
    of
        {ok,_,[[Uid]]} -> Uid;
        _ -> 'undefined'
    end.

-spec account_balance(ne_binary()) -> any().
account_balance(AccountId) ->
    UUID = lbuid_by_uuid(AccountId),
    case mysql_poolboy:query(?LB_MYSQL_POOL
                            ,<<"SELECT COALESCE(sum(balance),0) FROM agreements  where uid = ? and agreements.archive = 0">>
                            ,[UUID])
    of
        {ok,_,[[Amount]]} -> Amount;
        _ -> 'undefined'
    end.

-spec get_main_agrm_id(ne_binary()) -> any().
get_main_agrm_id(AccountId) ->
    UUID = lbuid_by_uuid(AccountId),
    case mysql_poolboy:query(?LB_MYSQL_POOL
                            ,<<"SELECT agrm_id from agreements where uid  = ? and oper_id = 1 limit 1">>
                            ,[UUID])
    of
        {ok,_,[[AgrmId]]} -> AgrmId;
        _ -> 'undefined'
    end.

-spec add_payment(ne_binary(), kz_json:object()) -> any().
add_payment(AccountId, JObj) ->
    EncodedDb = kz_json:get_value(<<"Database">>, JObj),
    DocId = kz_json:get_value(<<"ID">>, JObj),
    {'ok', Doc} = kz_datamgr:open_doc(EncodedDb, DocId),
    case kz_json:get_binary_value(<<"pvt_reason">>, Doc) of
        <<"wire_transfer">> ->
            AgrmId = get_main_agrm_id(AccountId),
            Summ = wht_util:units_to_dollars(kz_json:get_integer_value(<<"pvt_amount">>, Doc, 0)),
            Receipt = kz_json:get_binary_value(<<"_id">>, Doc, <<>>),
            Comment = kz_json:get_binary_value(<<"description">>, Doc, <<>>),
            lb_http:add_payment(AgrmId, Summ, Receipt, Comment, AccountId);
        _ ->
            'ok'
    end.

-spec sync_onbill_lb_info(ne_binary(), kz_json:object()) -> any().
sync_onbill_lb_info(AccountId, JObj) ->
    EncodedDb = kz_json:get_value(<<"Database">>, JObj),
    {'ok', Doc} = kz_datamgr:open_doc(EncodedDb, <<"onbill">>),
    case lbuid_by_uuid(AccountId) of
        'undefined' ->
            create_lb_account(AccountId, Doc),
            timer:sleep(1000),
            update_lb_account(lbuid_by_uuid(AccountId), AccountId, Doc);
        UID ->
            update_lb_account(UID, AccountId, Doc)
    end.

-spec create_lb_account(ne_binary(), kz_json:object()) -> any().
create_lb_account(AccountId, _Doc) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    [Login|_] = binary:split(kz_account:realm(AccountJObj), <<".">>),
    lb_http:soap_create_account(AccountId, Login, kz_binary:rand_hex(7), 1).

-spec update_lb_account(integer(), ne_binary(), kz_json:object()) -> any().
update_lb_account(UID, _AccountId, Doc) ->
    AccountName = kz_json:get_binary_value(<<"account_name">>, Doc, <<>>),
    INN = kz_json:get_binary_value(<<"account_inn">>, Doc, <<>>),
    KPP = kz_json:get_binary_value(<<"account_kpp">>, Doc, <<>>),
    mysql_poolboy:query(?LB_MYSQL_POOL
                       ,<<"UPDATE `billing`.`accounts` SET name = ?, inn = ?, kpp = ? WHERE accounts.uid = ?">>
                       ,[AccountName, INN, KPP, UID]).
    

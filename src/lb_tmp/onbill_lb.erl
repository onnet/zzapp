-module(onbill_lb).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([maybe_mysql_child/0
        ,lbuid_by_uuid/1
        ,account_balance/1
        ,get_main_agrm_id/1
        ,add_payment/2
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
    AgrmId = get_main_agrm_id(AccountId),
    Summ = wht_util:units_to_dollars(kz_json:get_integer_value(<<"pvt_amount">>, Doc, 0)),
    Receipt = kz_json:get_binary_value(<<"_id">>, Doc, <<>>),
    Comment = kz_json:get_binary_value(<<"description">>, Doc, <<>>),
    lb_http:add_payment(AgrmId, Summ, Receipt, Comment, AccountId).

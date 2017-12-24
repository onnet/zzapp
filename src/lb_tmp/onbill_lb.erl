-module(onbill_lb).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([
     maybe_mysql_child/0
]).

-include_lib("onbill.hrl").

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
            [mysql_poolboy:child_spec('lb_mysql', PoolOptions, MySqlOptions)];
        'false' ->
            []
    end.

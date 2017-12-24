-module(onbill_lb).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([
     maybe_mysql_child/0
]).

-include_lib("onbill.hrl").

maybe_mysql_child() ->
    PoolOptions  = [{size, 10}, {max_overflow, 20}],
    MySqlOptions = [{host, "host"}, {user, "zotonic"}, {password, "pwd"}, {database, "dbname"}],
    [mysql_poolboy:child_spec(lb_mysql, PoolOptions, MySqlOptions)].

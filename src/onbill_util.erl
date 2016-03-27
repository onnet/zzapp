-module(onbill_util).

-export([create/1
        ]).

-include("onbill.hrl").

-spec create(ne_binary()) -> 'ok'.
create(Db) when is_binary(Db) ->
    do_create(Db, couch_mgr:db_exists(Db)).

-spec do_create(ne_binary(), boolean()) -> 'ok'.
do_create(_Db, 'true') -> 'ok';
do_create(Db, 'false') ->
    lager:debug("create Db ~p", [Db]),
    _ = couch_mgr:db_create(Db).



-module(kt_onbill).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ]).

%% Verifiers
-export([
        ]).

%% Appliers
-export([current_state/2
        ]).

-include_lib("tasks/src/tasks.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(CATEGORY, "onbill").
-define(ACTIONS, [<<"current_state">>
                 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).


-spec output_header(ne_binary()) -> kz_csv:row().
output_header(<<"current_state">>) ->
    [<<"account_id">>
    ,<<"account_name">>
    ,<<"realm">>
    ,<<"is_enabled">>
    ,<<"is_reseller">>
    ,<<"descendants_count">>
    ,<<"current_service_status">>
    ,<<"current_balance">>
    ,<<"estimated_monthly_total">>
    ,<<"users">>
    ,<<"registered_devices">>
    ,<<"devices">>
    ].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_proplist().
action(<<"current_state">>) ->
    [{<<"description">>, <<"List per-month descendant accounts quantities">>}
    ,{<<"doc">>, <<"Line1.\n"
                   "Line 2.\n"
                 >>}
    ].

%%% Verifiers


%%% Appliers

-spec current_state(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
current_state(#{account_id := AccountId}, init) ->
    {'ok', get_descendants(AccountId)};
current_state(_, []) -> stop;
current_state(_, [SubAccountId | DescendantsIds]) ->
    {'ok', JObj} = kz_account:fetch(SubAccountId),
    Realm = kz_account:realm(JObj),
    Services = kz_services:fetch(SubAccountId),
    {[SubAccountId
     ,kz_account:name(JObj)
     ,Realm
     ,kz_account:is_enabled(JObj)
     ,kz_account:is_reseller(JObj)
     ,descendants_count(SubAccountId)
     ,onbill_util:current_service_status(SubAccountId)
     ,onbill_util:current_account_dollars(SubAccountId)
     ,onbill_bk_util:current_usage_amount(SubAccountId)
     ,kz_services:category_quantity(<<"users">>, Services)
     ,count_registrations(Realm)
     ,kz_services:category_quantity(<<"devices">>, Services)
     ], DescendantsIds}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_descendants(ne_binary()) -> ne_binaries().
get_descendants(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.

-spec descendants_count(ne_binary()) -> integer().
descendants_count(AccountId) ->
    ViewOptions = [{'group_level', 1}
                   | props:delete('group_level', [{'key', AccountId}])
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants_count">>, ViewOptions) of
        {'ok', [JObj|_]} -> kz_json:get_value(<<"value">>, JObj);
        {'ok', []} -> 0;
        {'error', _} -> 0
    end.

count_registrations(Realm) ->
    Req = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, []}
          ,{<<"Count-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = kapps_util:amqp_pool_request(Req
                                          ,fun kapi_registration:publish_query_req/1
                                          ,fun kapi_registration:query_resp_v/1
                                          ),
    case ReqResp of
        {'error', _E} -> lager:debug("no resps found: ~p", [_E]), 0;
        {'ok', JObj} -> kz_json:get_integer_value(<<"Count">>, JObj, 0);
        {'timeout', _} -> lager:debug("timed out query for counting regs"), 0
  end.

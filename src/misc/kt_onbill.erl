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
        ,periodic_fees/2
        ,generate_docs/2
        ,sync_onbills/2
        ,is_allowed/1
        ]).

-include_lib("tasks/src/tasks.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").
-include("onbill.hrl").

-define(CATEGORY, "onbill").
-define(ACTIONS, [<<"current_state">>
                 ,<<"periodic_fees">>
                 ,<<"generate_docs">>
                 ,<<"sync_onbills">>
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
    ,<<"billing_day">>
    ,<<"current_service_status">>
    ,<<"allow_postpay">>
    ,<<"max_postpay">>
    ,<<"current_balance">>
    ,<<"estimated_monthly_total">>
    ,<<"users">>
    ,<<"registered_devices">>
    ,<<"devices">>
    ];
output_header(<<"periodic_fees">>) ->
    [<<"service_id">>
    ,<<"name">>
    ,<<"rate">>
    ];
output_header(<<"generate_docs">>) ->
    [<<"account_id">>
    ,<<"name">>
    ];
output_header(<<"sync_onbills">>) ->
    [<<"account_id">>
    ,<<"name">>
    ,<<"state">>
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
    [{<<"description">>, <<"List current descendant accounts state">>}
    ,{<<"doc">>, <<"Just an experimentsl feature.\n"
                   "No additional parametres needed.\n"
                 >>}
    ];

action(<<"periodic_fees">>) ->
    [{<<"description">>, <<"List configured periodic fees">>}
    ,{<<"doc">>, <<"Just an experimentsl feature.\n"
                   "No additional parametres needed.\n"
                 >>}
    ];

action(<<"generate_docs">>) ->
    [{<<"description">>, <<"Generate invoices for children">>}
    ,{<<"doc">>, <<"Just an experimentsl feature." >>}
    ];

action(<<"sync_onbills">>) ->
    [{<<"description">>, <<"Syncronize account's onbill docs with onbill database">>}
    ,{<<"doc">>, <<"Just an experimentsl feature.">>}
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
    {AllowPostpay, MaxPostpay} =
        case onbill_util:maybe_allow_postpay(SubAccountId) of
            'false' -> {'false', 0};
            {'true', Max} -> {'true', Max}
        end,
    {[SubAccountId
     ,kz_account:name(JObj)
     ,Realm
     ,kz_account:is_enabled(JObj)
     ,kz_account:is_reseller(JObj)
     ,descendants_count(SubAccountId)
     ,onbill_util:billing_day(SubAccountId)
     ,onbill_util:current_service_status(SubAccountId)
     ,AllowPostpay
     ,wht_util:units_to_dollars(MaxPostpay)
     ,onbill_util:current_account_dollars(SubAccountId)
     ,estimated_monthly_total(SubAccountId)
     ,kz_services:category_quantity(<<"users">>, Services)
     ,count_registrations(Realm)
     ,kz_services:category_quantity(<<"devices">>, Services)
     ], DescendantsIds}.

-spec periodic_fees(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
periodic_fees(#{account_id := AccountId}, init) ->
    JObj = kz_service_plan:fetch(<<"onnet_periodic_fees">>, AccountId),
    Keys = kz_json:get_keys([<<"plan">>,<<"periodic_fees">>],JObj),
    {'ok', Keys};
periodic_fees(_, []) -> stop;
periodic_fees(#{account_id := AccountId}, [Key | Keys]) ->
    JObj = kz_service_plan:fetch(<<"onnet_periodic_fees">>, AccountId),
    {[Key
     ,kz_json:get_value([<<"plan">>,<<"periodic_fees">>,Key,<<"name">>],JObj)
     ,kz_json:get_value([<<"plan">>,<<"periodic_fees">>,Key,<<"rate">>],JObj)
     ], Keys}.

-spec generate_docs(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
generate_docs(#{account_id := AccountId}, init) ->
    {'ok', get_children(AccountId)};
generate_docs(_, []) -> stop;
generate_docs(_, [SubAccountId | DescendantsIds]) ->
    {'ok', JObj} = kz_account:fetch(SubAccountId),
    {{Year,Month,Day},{_,_,_}} = calendar:universal_time(),
    {PSYear,PSMonth,PSDay} = onbill_util:previous_period_start_date(SubAccountId, Year, Month, Day),
    {PEYear,PEMonth,PEDay} = onbill_util:period_end_date(SubAccountId, PSYear, PSMonth, PSDay),
    try 
        onbill_docs:generate_docs(SubAccountId, PEYear, PEMonth, PEDay)
    catch
        E ->
            lager:info("try/catch generate_docs Error: ~p", [E]),
            timer:sleep(50000),
            onbill_docs:generate_docs(SubAccountId, PEYear, PEMonth, PEDay)
    end,
    {[SubAccountId
     ,kz_account:name(JObj)
     ]
    ,DescendantsIds
    }.

-spec sync_onbills(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
sync_onbills(#{account_id := AccountId}, init) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    DbName = ?ONBILL_DB(ResellerId),
    _ = onbill_util:maybe_add_design_doc(DbName, <<"search">>),
    {'ok', get_children(AccountId)};
sync_onbills(_, []) -> stop;
sync_onbills(_, [SubAccountId | DescendantsIds]) ->
    {'ok', JObj} = kz_account:fetch(SubAccountId),
    case replicate_onbill_doc(SubAccountId) of
        {'ok', _} ->
            {[SubAccountId ,kz_account:name(JObj), <<"exists">>] ,DescendantsIds };
        _ ->
            {[SubAccountId ,kz_account:name(JObj), <<"absent">>] ,DescendantsIds }
    end.


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

-spec get_children(ne_binary()) -> ne_binaries().
get_children(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_children">>, ViewOptions) of
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

estimated_monthly_total(AccountId) ->
    case onbill_util:is_service_plan_assigned(AccountId) of
        'true' -> onbill_bk_util:current_usage_amount(AccountId);
        'false' -> 'no_service_plan_assigned'
    end.

-spec is_allowed(kz_tasks:extra_args()) -> boolean().
is_allowed(ExtraArgs) ->
    AuthAccountId = maps:get('auth_account_id', ExtraArgs),
    AccountId = maps:get('account_id', ExtraArgs),
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    {'ok', AuthAccountDoc} = kz_account:fetch(AuthAccountId),
    kz_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true')
        andalso kz_account:is_reseller(AccountDoc)
        orelse kz_account:is_superduper_admin(AuthAccountDoc).

-spec replicate_onbill_doc(ne_binary()) ->
                                          {'ok', kz_json:object()} |
                                          {'error', any()}.
replicate_onbill_doc(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, ?ONBILL_DOC) of
        {ok, Doc} ->
            ResellerId = kz_services:find_reseller_id(AccountId),
            DbName = ?ONBILL_DB(ResellerId),
            JObj = kz_json:set_value(<<"_id">>, AccountId, Doc),
            onbill_util:check_db(DbName),
            case kz_datamgr:lookup_doc_rev(DbName, AccountId) of
                {'ok', Rev} ->
                    kz_datamgr:ensure_saved(DbName, kz_doc:set_revision(JObj, Rev));
                _Else ->
                    kz_datamgr:ensure_saved(DbName, kz_doc:delete_revision(JObj))
            end;
        E ->
            E
    end.

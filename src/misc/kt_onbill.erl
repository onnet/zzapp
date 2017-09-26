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
        ,import_accounts/3
        ,add_users/3
        ,is_allowed/1
        ]).

-include_lib("tasks/src/tasks.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(CATEGORY, "onbill").
-define(ACTIONS, [<<"current_state">>
                 ,<<"import_accounts">>
                 ,<<"add_users">>
                 ]).

-define(IMPORT_ACCOUNTS_DOC_FIELDS
       ,[<<"account_name">>
        ,<<"realm">>
        ,<<"users">>
        ]).

-define(IMPORT_ACCOUNTS_MANDATORY_FIELDS
       ,[<<"account_name">>
        ]).

-define(ADD_USERS_DOC_FIELDS
       ,[<<"account_id">>
        ,<<"users">>
        ]).

-define(ACCOUNT_REALM_SUFFIX
       ,kapps_config:get_binary(<<"crossbar.accounts">>, <<"account_realm_suffix">>, <<"sip.onnet.su">>)).

-define(MK_USER,
    {[{<<"call_forward">>,
       {[{<<"substitute">>,false},
         {<<"enabled">>,false},
         {<<"require_keypress">>,false},
         {<<"keep_caller_id">>,false},
         {<<"direct_calls_only">>,false}]}},
      {<<"enabled">>, 'true'},
      {<<"priv_level">>,<<"user">>},
      {<<"vm_to_email_enabled">>,true},
      {<<"fax_to_email_enabled">>,true},
      {<<"verified">>,false},
      {<<"timezone">>,<<"UTC">>},
      {<<"record_call">>,false}
     ]}).

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

action(<<"import_accounts">>) ->
    Mandatory = ?IMPORT_ACCOUNTS_MANDATORY_FIELDS,
    Optional = ?IMPORT_ACCOUNTS_DOC_FIELDS -- Mandatory,

    [{<<"description">>, <<"Bulk-create accounts using account_names list">>}
    ,{<<"doc">>, <<"Creates accounts from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ];

action(<<"add_users">>) ->
    [{<<"description">>, <<"Bulk create users using account_id,emails list">>}
    ,{<<"doc">>, <<"Creates users for accounts from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, ?ADD_USERS_DOC_FIELDS}
    ,{<<"optional">>, []}
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

-spec import_accounts(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
import_accounts(ExtraArgs, init, Args) ->
    kz_datamgr:suppress_change_notice(),
    IterValue = sets:new(),
    import_accounts(ExtraArgs, IterValue, Args);
import_accounts(#{account_id := ResellerId
        ,auth_account_id := _AuthAccountId
        }
      ,_AccountIds
      ,_Args=#{<<"account_name">> := AccountName
             ,<<"users">> := UserString
             }
      ) ->
    Realm = <<AccountName/binary, ".", (?ACCOUNT_REALM_SUFFIX)/binary>>,
    Context = create_account(ResellerId, AccountName, Realm),
    case cb_context:resp_status(Context) of
        'success' ->
            kz_util:spawn(fun cb_onbill_signup:create_default_callflow/1, [Context]),
            RespData = cb_context:resp_data(Context),
            AccountId = kz_json:get_value(<<"id">>, RespData),
            case UserString of
                'undefined' ->
                    AccountId;
                _ ->
                    Users = binary:split(re:replace(UserString, "\\s+", "", [global,{return,binary}])
                                        ,[<<",">>,<<";">>]),
                    create_users(AccountId, Users, Context),
                    AccountId
            end;
        _ ->
            'account_not_created'
    end.

-spec add_users(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
add_users(ExtraArgs, init, Args) ->
    kz_datamgr:suppress_change_notice(),
    IterValue = sets:new(),
    add_users(ExtraArgs, IterValue, Args);
add_users(#{account_id := ResellerId
        ,auth_account_id := _AuthAccountId
        }
      ,_AccountIds
      ,_Args=#{<<"AccountId">> := AccountId
             ,<<"users">> := UserString
             }
      ) ->
    Context = cb_context:set_account_id(cb_context:new(), ResellerId),
    case UserString of
        'undefined' ->
            AccountId;
        _ ->
            Users = binary:split(re:replace(UserString, "\\s+", "", [global,{return,binary}])
                                ,[<<",">>,<<";">>]),
            create_users(AccountId, Users, Context),
            AccountId
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

create_account(ResellerId, AccountName, Realm) ->
    Tree = crossbar_util:get_tree(ResellerId) ++ [ResellerId],
    Props = [{<<"pvt_type">>, kz_account:type()}
            ,{<<"name">>, AccountName}
            ,{<<"realm">>, Realm}
            ,{<<"timezone">>,<<"Europe/Moscow">>}
            ,{<<"language">>,<<"ru-ru">>}
            ,{<<"pvt_tree">>, Tree}
            ],
    Ctx1 = cb_context:set_account_id(cb_context:new(), ResellerId),
    Ctx2 = cb_context:set_doc(Ctx1, kz_json:set_values(Props, kz_json:new())),
    cb_accounts:put(Ctx2).

create_users(_AccountId, [], _Context) -> 'ok';
create_users(AccountId, [UserName|Users], Context) -> 
    UserPassword = kz_binary:rand_hex(10),
    Props = props:filter_empty([
         {[<<"username">>], UserName}
        ,{[<<"first_name">>], <<"Firstname">>}
        ,{[<<"last_name">>], <<"Surname">>}
        ,{[<<"email">>], UserName}
        ,{[<<"password">>], UserPassword}
        ,{[<<"timezone">>],<<"Europe/Moscow">>}
        ,{[<<"priv_level">>], <<"admin">>}
        ]),
    UserData = kz_json:set_values(Props, ?MK_USER),
    Ctx1 = cb_context:set_account_id(Context, AccountId),
    Ctx2 = cb_context:set_doc(Ctx1, UserData),
    Ctx3 = cb_context:set_req_data(Ctx2, UserData),
    Ctx4 = cb_users_v1:create_user(cb_context:set_accepting_charges(Ctx3)),
    send_email(Ctx4),
    timer:sleep(1000),
    create_users(AccountId, Users, Context).

-spec send_email(cb_context:context()) -> 'ok'.
send_email(Context) ->
    Doc = cb_context:doc(Context),
    ReqData = cb_context:req_data(Context),
    Req = [{<<"Account-ID">>, cb_context:account_id(Context)}
          ,{<<"User-ID">>, kz_doc:id(Doc)}
          ,{<<"Password">>, kz_json:get_value(<<"password">>, ReqData)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_notify_publisher:cast(Req, fun kapi_notifications:publish_new_user/1).


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
        ,import_periodic_fees/3
        ,import_accounts/3
        ,import_onbill_data1/3
        ,generate_docs/2
        ,is_allowed/1
        ,maybe_format_address_element/2
        ]).

-include_lib("tasks/src/tasks.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(ONBILL_DOC, <<"onbill">>).
-define(CATEGORY, "onbill").
-define(ACTIONS, [<<"current_state">>
                 ,<<"periodic_fees">>
                 ,<<"import_periodic_fees">>
                 ,<<"import_accounts">>
                 ,<<"import_onbill_data1">>
                 ,<<"generate_docs">>
                 ]).

-define(IMPORT_PERIODIC_FEES_DOC_FIELDS
       ,[<<"account_id">>
        ,<<"service_id">>
        ,<<"quantity">>
        ]).

-define(IMPORT_PERIODIC_FEES_MANDATORY_FIELDS
       ,[<<"account_id">>
        ,<<"service_id">>
        ,<<"quantity">>
        ]).


-define(IMPORT_ACCOUNTS_DOC_FIELDS
       ,[<<"account_name">>
        ,<<"realm">>
        ,<<"users">>
        ]).

-define(IMPORT_ACCOUNTS_MANDATORY_FIELDS
       ,[<<"account_name">>
        ]).

-define(IMPORT_ONBILL_DATA1
       ,[<<"account_id">>
        ,<<"account_name">>
        ,<<"account_inn">>
        ,<<"account_kpp">>
        ,<<"prepaid">>
        ,<<"billing_address_line1">>
        ,<<"billing_address_line2">>
        ,<<"billing_address_line3">>
        ,<<"agrm_number">>
        ,<<"agrm_date">>
        ,<<"agrm_type_id">>
        ]).

-define(IMPORT_MANDATORY_ONBILL_DATA1
       ,[<<"account_id">>
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
    ];
output_header(<<"periodic_fees">>) ->
    [<<"service_id">>
    ,<<"name">>
    ,<<"rate">>
    ];
output_header(<<"generate_docs">>) ->
    [<<"account_id">>
    ,<<"name">>
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

action(<<"import_periodic_fees">>) ->
    Mandatory = ?IMPORT_PERIODIC_FEES_MANDATORY_FIELDS,
    Optional = ?IMPORT_PERIODIC_FEES_DOC_FIELDS -- Mandatory,

    [{<<"description">>, <<"Bulk-import using periodic fees list">>}
    ,{<<"doc">>, <<"Assigning periodic fees from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
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

action(<<"import_onbill_data1">>) ->
    Mandatory = ?IMPORT_MANDATORY_ONBILL_DATA1,
    Optional = ?IMPORT_ONBILL_DATA1 -- Mandatory,

    [{<<"description">>, <<"Bulk-import accounts company data for invoice generating">>}
    ,{<<"doc">>, <<"Imports onbill data from file">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, Mandatory}
    ,{<<"optional">>, Optional}
    ];

action(<<"generate_docs">>) ->
    [{<<"description">>, <<"Generate invoices for children">>}
    ,{<<"doc">>, <<"Just an experimentsl feature.\n"
                   "Month/Year of the end of billing period needed.\n"
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

-spec import_periodic_fees(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
import_periodic_fees(ExtraArgs=#{account_id := ResellerId}, init, Args) ->
    remove_periodic_fees_from_db(get_descendants(ResellerId)),
    kz_datamgr:suppress_change_notice(),
    IterValue = sets:new(),
    import_periodic_fees(ExtraArgs, IterValue, Args);
import_periodic_fees(_
                    ,_
                    ,#{<<"account_id">> := AccountId
                      ,<<"service_id">> := <<"phone_line_649">>
                      ,<<"quantity">> := Quantity
                      }
      ) ->
    DbName = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(DbName, <<"limits">>) of
        {ok, Doc} ->
            NewDoc = kz_json:set_value(<<"twoway_trunks">>, kz_term:to_integer(Quantity), Doc),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            AccountId;
        {'error', 'not_found'} ->
            Values =
                props:filter_undefined(
                    [{<<"_id">>, <<"limits">>}
                    ,{<<"pvt_type">>, <<"limits">>}
                    ,{<<"twoway_trunks">>, kz_term:to_integer(Quantity)}
                    ]),
            NewDoc = kz_json:set_values(Values ,kz_json:new()),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            AccountId;
        _ ->
            'onbill_data_not_added'
    end;
import_periodic_fees(_
                    ,_
                    ,#{<<"account_id">> := AccountId
                      ,<<"service_id">> := ServiceId
                      ,<<"quantity">> := Quantity
                      }
      ) ->
    DbName = kz_util:format_account_id(AccountId, 'encoded'),
    ServiceStarts = calendar:datetime_to_gregorian_seconds({onbill_util:period_start_date(AccountId), {0,0,0}}),
    Values =
        props:filter_undefined(
            [{<<"_id">>, kz_datamgr:get_uuid()}
            ,{<<"pvt_type">>, <<"periodic_fee">>}
            ,{<<"service_id">>, ServiceId}
            ,{<<"quantity">>, Quantity}
            ,{<<"service_starts">>, ServiceStarts}
            ]),
    kz_datamgr:save_doc(DbName,kz_json:from_list(Values)),
    AccountId.

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

-spec import_onbill_data1(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
import_onbill_data1(ExtraArgs, init, Args) ->
    case is_allowed(ExtraArgs) of
        'true' ->
            lager:info("import_onbill_data1 is allowed, continuing"),
            kz_datamgr:suppress_change_notice(),
            IterValue = sets:new(),
            import_onbill_data1(ExtraArgs, IterValue, Args);
        'false' ->
            lager:warning("import_onbill_data1 is forbidden for account ~s, auth account ~s"
                         ,[maps:get('account_id', ExtraArgs)
                          ,maps:get('auth_account_id', ExtraArgs)
                          ]
                         ),
            {<<"task execution is forbidden">>, 'stop'}
    end;
import_onbill_data1(#{account_id := _ResellerId
        ,auth_account_id := _AuthAccountId
        }
      ,_AccountIds
      ,_Args=#{<<"account_id">> := AccountId
              ,<<"account_name">> := AccountName
              ,<<"account_inn">> := AccountINN
              ,<<"account_kpp">> := AccountKPP
              ,<<"prepaid">> := _Prepaid
              ,<<"billing_address">> := BillingAddress
              ,<<"agrm_number">> := AgrmNumber
              ,<<"agrm_date">> := AgrmDate
              ,<<"agrm_type_id">> := AgrmTypeId
              }
      ) ->
lager:info("IAM BillingAddress: ~p",[BillingAddress]),
lager:info("IAM AgrmNumber: ~p",[AgrmNumber]),
lager:info("IAM AgrmDate: ~p",[AgrmDate]),
    case binary:split(BillingAddress, [<<"^*^">>], [global]) of
        [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] -> ok;
        [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, _, A11] -> ok
    end,
lager:info("IAM A1: ~p",[A1]),
lager:info("IAM A2: ~p",[A2]),
lager:info("IAM A3: ~p",[A3]),
lager:info("IAM A4: ~p",[A4]),
lager:info("IAM A5: ~p",[A5]),
lager:info("IAM A6: ~p",[A6]),
lager:info("IAM A7: ~p",[A7]),
lager:info("IAM A8: ~p",[A8]),
lager:info("IAM A9: ~p",[A9]),
lager:info("IAM A10: ~p",[A10]),
lager:info("IAM A11: ~p",[A11]),
    AA4 = case A2 == A4 of
              'true' -> <<>>;
              'false' -> A4
          end,
    Values = props:filter_empty(
        [{<<"_id">>, ?ONBILL_DOC}
        ,{<<"pvt_type">>, ?ONBILL_DOC}
        ,{<<"pvt_account_id">>, AccountId}
        ,{<<"account_name">>, AccountName}
        ,{<<"account_inn">>, AccountINN}
        ,{<<"account_kpp">>, AccountKPP}
        ,{[<<"billing_address">>,<<"line1">>]
         ,maybe_format_address_element([A1, A2], A11)
         }
        ,{[<<"billing_address">>,<<"line2">>]
         ,maybe_format_address_element([AA4, A5, A6], A3)
         }
        ,{[<<"billing_address">>,<<"line3">>]
         ,maybe_format_address_element([A8, A9, A10], A7)
         }
        ] ++ agrm_vals(AgrmNumber, AgrmDate, AgrmTypeId)),
    DbName = kz_util:format_account_id(AccountId,'encoded'),
    case kz_datamgr:open_doc(DbName, ?ONBILL_DOC) of
        {ok, Doc} ->
            NewDoc = kz_json:set_values(Values, Doc),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            AccountId;
        {'error', 'not_found'} ->
            NewDoc = kz_json:set_values(Values ,kz_json:new()),
            kz_datamgr:ensure_saved(DbName, NewDoc),
            AccountId;
        _ ->
            'onbill_data_not_added'
    end.

-spec generate_docs(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
generate_docs(#{account_id := AccountId}, init) ->
    {'ok', get_children(AccountId)};
generate_docs(_, []) -> stop;
generate_docs(_, [SubAccountId | DescendantsIds]) ->
  lager:info("IAMTASKS generate_docs SubAccountId: ~p",[SubAccountId]),
  lager:info("IAMTASKS generate_docs DescendantsIds: ~p",[DescendantsIds]),
    {'ok', JObj} = kz_account:fetch(SubAccountId),
  lager:info("IAMTASKS generate_docs fetch JObj: ~p",[JObj]),
    {{Year,Month,Day},{_,_,_}} = calendar:universal_time(),
    {PSYear,PSMonth,PSDay} = onbill_util:previous_period_start_date(SubAccountId, Year, Month, Day),
    {PEYear,PEMonth,PEDay} = onbill_util:period_end_date(SubAccountId, PSYear, PSMonth, PSDay),
    onbill_docs:generate_docs(SubAccountId, PEYear, PEMonth, PEDay),
  lager:info("IAMTASKS generate_docs kz_account:name JObj: ~p",[kz_account:name(JObj)]),
    {[SubAccountId
     ,kz_account:name(JObj)
     ]
    ,DescendantsIds
    }.

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
    Ctx4 = cb_users_v1:put(Ctx3),
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

-spec maybe_format_address_element(any(), any()) -> 'ok'.
maybe_format_address_element([], Acc) ->
    Acc;
maybe_format_address_element([<<>>], Acc) ->
    Acc;
maybe_format_address_element([H], <<>>) ->
    H;
maybe_format_address_element([H], Acc) ->
    <<Acc/binary, ", ", H/binary>>;
maybe_format_address_element([<<>>|T], Acc) ->
    maybe_format_address_element(T, Acc);
maybe_format_address_element([H|T], Acc) when Acc == <<>> ->
    maybe_format_address_element(T, <<H/binary>>);
maybe_format_address_element([H|T], Acc) ->
    maybe_format_address_element(T, <<Acc/binary, ", ", H/binary>>).

format_agrm_date(<<YYYY:4/binary, "-", MM:2/binary, "-", DD:2/binary>>) ->
    <<DD/binary, ".", MM/binary, ".", YYYY/binary>>;
format_agrm_date(AgrmDate) ->
    AgrmDate.

agrm_vals(_, _, 'undefined') ->
    [];
agrm_vals(<<"I0#", AgrmNumber/binary>>, AgrmDate, AgrmTypeId) ->
    agrm_vals(AgrmNumber, AgrmDate, AgrmTypeId);
agrm_vals(<<"0WG#", AgrmNumber/binary>>, AgrmDate, AgrmTypeId) ->
    agrm_vals(AgrmNumber, AgrmDate, AgrmTypeId);
agrm_vals(AgrmNumber, AgrmDate, <<"1">>) ->
    [{[<<"agrm">>,<<"onnet">>,<<"number">>], AgrmNumber}
    ,{[<<"agrm">>,<<"onnet">>,<<"date">>], format_agrm_date(AgrmDate)}];
agrm_vals(AgrmNumber, AgrmDate, <<"2">>) ->
    [{[<<"agrm">>,<<"beeline_spb">>,<<"number">>], <<"I0#", AgrmNumber/binary>>}
    ,{[<<"agrm">>,<<"beeline_spb">>,<<"date">>], format_agrm_date(AgrmDate)}];
agrm_vals(AgrmNumber, AgrmDate, <<"3">>) ->
    [{[<<"agrm">>,<<"beeline_msk">>,<<"number">>], <<"0WG#", AgrmNumber/binary>>}
    ,{[<<"agrm">>,<<"beeline_msk">>,<<"date">>], format_agrm_date(AgrmDate)}].

remove_periodic_fees_from_db([]) ->
    'ok';
remove_periodic_fees_from_db([DescendantId | DescendantsIds]) ->
    Services = kz_services:delete_service_plan(<<"voip_service_plan">>, kz_services:fetch(DescendantId)),
    kz_services:save(kz_services:add_service_plan(<<"onnet_periodic_fees">>, Services)),
    DbName = kz_util:format_account_id(DescendantId, 'encoded'),
    case kz_datamgr:get_result_ids(DbName, <<"periodic_fees/crossbar_listing">>, []) of
        {'ok', Ids} ->
            kz_datamgr:del_docs(DbName, Ids);
        {'error', _R} ->
            lager:debug("unable to get periodic_fees docs of ~s: ~p", [DescendantId, _R])
    end,
    timer:sleep(100),
    remove_periodic_fees_from_db(DescendantsIds).


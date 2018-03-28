-module(onbill_notifications).

-export([send_account_update/3
        ,init/0
        ,maybe_send_account_updates/2
        ,customer_update_databag/1
        ,maybe_send_service_suspend_update/1
        ,maybe_send_trial_has_expired_update/1
        ]).

-include("onbill.hrl").

-define(MRC_APPROACHING_KEY, <<"mrc_approaching">>).
-define(SERVICE_SUSPEND_KEY, <<"service_suspend">>).
-define(TRIAL_HAS_EXPIRED_KEY, <<"trial_has_expired">>).
-define(ONBILL_NOTIFICATION_ENABLED, <<"onbill_notification_enabled">>).
-define(KEY_SENT(Key), [<<"notifications">>, Key, <<"sent">>]).
-define(KEY_ENABLED(Key), [<<"notifications">>, Key, <<"enabled">>]).
-define(KEY_TSTAMP(Key), [<<"notifications">>, Key, <<"last_notification">>]).
-define(KEY_PERIOD(Key),
        kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<Key/binary, "_period_days">>, 5)).
-define(KEY_REPEAT(Key),
        kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<Key/binary, "_repeat_s">>, 1 * ?SECONDS_IN_DAY)).

-define(MACRO_VALUE(Key, Label, Name, Description)
       ,{Key
        ,kz_json:from_list([{<<"i18n_label">>, Label}
                           ,{<<"friendly_name">>, Name}
                           ,{<<"description">>, Description}
                           ])
        }).

-define(USER_MACROS
       ,[?MACRO_VALUE(<<"user.first_name">>, <<"user_first_name">>, <<"First Name">>, <<"First name of the user">>)
        ,?MACRO_VALUE(<<"user.last_name">>, <<"user_last_name">>, <<"Last Name">>, <<"Last name of the user">>)
        ,?MACRO_VALUE(<<"user.email">>, <<"user_email">>, <<"Email">>, <<"Email of the user">>)
        ,?MACRO_VALUE(<<"user.timezone">>, <<"user_timezone">>, <<"Timezone">>, <<"Timezone of the user">>)
        ,?MACRO_VALUE(<<"user.username">>, <<"username">>, <<"Username">>, <<"Username">>)
        ]).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
          ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
           | ?USER_MACROS
          ])
       ).

-define(EMAIL_SPECIFIED, <<"specified">>).
-define(EMAIL_ORIGINAL, <<"original">>).
-define(EMAIL_ADMINS, <<"admins">>).

-define(CONFIGURED_EMAILS(Type, Addresses)
       ,kz_json:from_list(
          props:filter_undefined(
            [{<<"type">>, Type}
            ,{<<"email_addresses">>, Addresses}
            ])
         )
       ).
-define(CONFIGURED_EMAILS(Type), kz_json:from_list([{<<"type">>, Type}])).

% -define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_CATEGORY, <<"onbill">>).
-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CRAWLER)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CRAWLER)).

-spec init() -> 'ok'.
init() ->
    mrc_init(),
    mrc_approaching_init(),
    service_suspended_init(),
    limits_set_to_zero_init(),
    trial_has_expired_init().

-spec mrc_init() -> 'ok'.
mrc_init() ->
    teletype_templates:init(?MRC_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"New billing period for {{databag.account.name}}">> }
                                                       ,{'category', ?TEMPLATE_CATEGORY}
                                                       ,{'friendly_name', <<"New billing period">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]).

-spec mrc_approaching_init() -> 'ok'.
mrc_approaching_init() ->
    teletype_templates:init(?MRC_APPROACHING_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"New billing period approaching for {{databag.account.name}}">> }
                                                       ,{'category', ?TEMPLATE_CATEGORY}
                                                       ,{'friendly_name', <<"New billing period approaching">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]).

-spec limits_set_to_zero_init() -> 'ok'.
limits_set_to_zero_init() ->
    teletype_templates:init(?LIMITS_SET_TO_ZERO_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"Trunks services canceled for {{databag.account.name}}">> }
                                                       ,{'category', ?TEMPLATE_CATEGORY}
                                                       ,{'friendly_name', <<"Trunks services canceled">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]).

-spec service_suspended_init() -> 'ok'.
service_suspended_init() ->
    teletype_templates:init(?SERVICE_SUSPENDED_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"Service suspended for {{databag.account.name}}">> }
                                                       ,{'category', ?TEMPLATE_CATEGORY}
                                                       ,{'friendly_name', <<"Service suspended">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]).

-spec trial_has_expired_init() -> 'ok'.
trial_has_expired_init() ->
    teletype_templates:init(?TRIAL_HAS_EXPIRED_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"Trial period has expired for {{databag.account.name}}">> }
                                                       ,{'category', ?TEMPLATE_CATEGORY}
                                                       ,{'friendly_name', <<"Trial has expired">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]).

-spec send_account_update(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_account_update(AccountId, TemplateId, DataBag) ->
    case kz_amqp_worker:call(build_customer_update_payload(AccountId, TemplateId, DataBag)
                            ,fun kapi_notifications:publish_customer_update/1
                            ,fun kapi_notifications:customer_update_v/1
                            )
    of
        {'ok', _Resp} ->
            lager:debug("published customer_update notification");
        {'error', _E} ->
            lager:debug("failed to publish_customer update notification: ~p", [_E])
    end.

-spec build_customer_update_payload(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
build_customer_update_payload(AccountId, TemplateId, DataBag) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    RecipientId =
        case onbill_notification_enabled(AccountId) of
            'true' -> AccountId;
            'false' -> ResellerId
        end,
    props:filter_empty(
      [{<<"Account-ID">>, ResellerId}
      ,{<<"Recipient-ID">>, RecipientId}
      ,{<<"Template-ID">>, TemplateId}
      ,{<<"DataBag">>, DataBag}
       | kz_api:default_headers(?OB_APP_NAME, ?OB_APP_VERSION)
      ]).

-spec maybe_send_account_updates(kz_term:ne_binary(), kz_account:doc()) -> 'ok'.
maybe_send_account_updates(AccountId, AccountJObj) ->
    case onbill_util:is_trial_account(AccountJObj) of
        'true' ->
            'ok';
        'false' ->
            case onbill_util:maybe_allow_postpay(AccountId) of
                'false' ->
                    maybe_new_billing_period_approaching(AccountId, AccountJObj);
                _ -> 'ok'
            end
    end.

-spec maybe_new_billing_period_approaching(kz_term:ne_binary(), kz_account:doc()) -> 'ok'.
maybe_new_billing_period_approaching(AccountId, AccountJObj) ->
    Timestamp = kz_time:current_tstamp(),
    Days_Before_MRC_Update = ?KEY_PERIOD(?MRC_APPROACHING_KEY),
    case onbill_util:days_left_in_period(AccountId, Timestamp) of
        DaysLeft when DaysLeft < Days_Before_MRC_Update ->
            case onbill_bk_util:current_usage_amount_in_units(AccountId)
                > onbill_util:current_balance(AccountId)
            of
                'true' ->
                    maybe_send_new_billing_period_approaching_update(AccountId
                                                                    ,AccountJObj
                                                                    ,key_enabled(?MRC_APPROACHING_KEY, AccountJObj));
                _ ->
                    'ok'
            end;
        _ -> 'ok'
    end.

-spec maybe_send_new_billing_period_approaching_update(kz_term:ne_binary(), kz_account:doc(), boolean()) -> 'ok'.
maybe_send_new_billing_period_approaching_update(AccountId, AccountJObj, 'true') ->
    case key_tstamp(?MRC_APPROACHING_KEY, AccountJObj) of
        MRC_ApproachingSent when is_number(MRC_ApproachingSent) ->
            Cycle = ?KEY_REPEAT(?MRC_APPROACHING_KEY),
            Diff = kz_time:current_tstamp() - MRC_ApproachingSent,
            case Diff >= Cycle of
               'true' ->
                   'ok' = send_account_update(AccountId, ?MRC_APPROACHING_TEMPLATE, customer_update_databag(AccountId)),
                   update_account_key_sent(?MRC_APPROACHING_KEY, AccountJObj);
               'false' ->
                   lager:debug("mrc approaching alert sent ~w seconds ago, repeats every ~w", [Diff, Cycle])
            end;
        _Else ->
            'ok' = send_account_update(AccountId, ?MRC_APPROACHING_TEMPLATE, customer_update_databag(AccountId)),
            update_account_key_sent(?MRC_APPROACHING_KEY, AccountJObj)
    end,
    'ok';
maybe_send_new_billing_period_approaching_update(AccountId, _AccountJObj, 'false') ->
    lager:debug("mrc approaching alert disabled for Account: ~p", [AccountId]).

-spec customer_update_databag(kz_term:ne_binary()) -> kz_json:object().
customer_update_databag(AccountId) ->
    Values = [{<<"reseller">>, reseller_info_databag(AccountId)}
             ,{<<"account">>, account_info_databag(AccountId)}
             ,{<<"services">>, services_info_databag(AccountId)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec reseller_info_databag(kz_term:ne_binary()) -> kz_json:object().
reseller_info_databag(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    {'ok', ResellerDoc} = kz_account:fetch(ResellerId),
    Values = [{<<"name">>, kz_account:name(ResellerDoc)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec account_info_databag(kz_term:ne_binary()) -> kz_json:object().
account_info_databag(AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    Values = [{<<"name">>, kz_account:name(AccountDoc)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec services_info_databag(kz_term:ne_binary()) -> kz_json:object().
services_info_databag(AccountId) ->
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    {'ok', Items} = kz_service_plans:create_items(ServicesJObj),
    ItemsList = onbill_bk_util:select_non_zero_items_list(Items, AccountId),
    ItemsCalculatedList = [onbill_bk_util:calc_item(ItemJObj, AccountId) || ItemJObj <- ItemsList],

    Timestamp = kz_time:current_tstamp(),
    DaysLeft =  onbill_util:days_left_in_period(AccountId, Timestamp),
    {NextPeriodYear, NextPeriodMonth, NextPeriodDay} = onbill_util:next_period_start_date(AccountId, Timestamp),

    ResellerVars = onbill_util:reseller_vars(AccountId),
    CurrencySign = kz_json:get_value(<<"currency_sign">>, ResellerVars, <<"£"/utf8>>),
               
    kz_json:from_list([{<<"total_amount">>, currency_sign:add_currency_sign(CurrencySign
                                                                           ,onbill_bk_util:items_amount(ItemsList, AccountId, 0.0))}
                      ,{<<"services_list">>, ItemsCalculatedList}
                      ,{<<"account_id">>, AccountId}
                      ,{<<"days_left">>, DaysLeft}
                      ,{<<"current_balance">>, currency_sign:add_currency_sign(CurrencySign
                                                                              ,onbill_util:current_account_dollars(AccountId))}
                      ,{<<"next_period_date">>, onbill_util:date_json(NextPeriodYear
                                                                     ,NextPeriodMonth
                                                                     ,NextPeriodDay)}
                      ,{<<"currency_short">>, kz_json:get_value(<<"currency_short">>, ResellerVars, <<"GBP">>)}
                      ,{<<"currency_sign">>, CurrencySign}
                      ]).

-spec update_account_key_sent(kz_term:ne_binary(), kz_account:doc()) -> 'ok'.
update_account_key_sent(Key, AccountJObj0) ->
    AccountJObj1 = set_key_sent(Key, AccountJObj0),
    AccountJObj2 = set_key_tstamp(Key, AccountJObj1),
    _ = kz_util:account_update(AccountJObj2),
   'ok'.

-spec onbill_notification_enabled(kz_term:ne_binary()) -> boolean().
onbill_notification_enabled(AccountId) ->
    kz_json:is_true(?ONBILL_NOTIFICATION_ENABLED, onbill_util:reseller_vars(AccountId)).

-spec set_key_sent(kz_term:ne_binary(), kz_account:doc()) -> kz_account:doc().
set_key_sent(Key, JObj) ->
    kz_json:set_value(?KEY_SENT(Key), 'true', JObj).

-spec key_enabled(kz_term:ne_binary(), kz_account:doc()) -> boolean().
key_enabled(Key, JObj) ->
    kz_json:is_true(?KEY_ENABLED(Key), JObj, 'true').

%-spec reset_key_enabled(kz_term:ne_binary(), kz_account:doc()) -> boolean().
%reset_key_enabled(Key, JObj) ->
%    kz_json:is_true(?KEY_ENABLED(Key), JObj, 'false').

-spec key_tstamp(kz_term:ne_binary(), kz_account:doc()) -> kz_term:api_number().
key_tstamp(Key, JObj) ->
    kz_json:get_integer_value(?KEY_TSTAMP(Key), JObj).

-spec set_key_tstamp(kz_term:ne_binary(), kz_account:doc()) -> kz_account:doc().
set_key_tstamp(Key, JObj) ->
    TStamp = kz_time:current_tstamp(),
    set_key_tstamp(Key, JObj, TStamp).

-spec set_key_tstamp(kz_term:ne_binary(), kz_account:doc(), number()) -> kz_account:doc().
set_key_tstamp(Key, JObj, TStamp) ->
    kz_json:set_value(?KEY_TSTAMP(Key), TStamp, JObj).

-spec maybe_send_service_suspend_update(kz_term:ne_binary()) -> any().
-spec maybe_send_service_suspend_update(kz_term:ne_binary(), kz_account:doc(), boolean()) -> any().
maybe_send_service_suspend_update(AccountId) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    maybe_send_service_suspend_update(AccountId, AccountJObj, key_enabled(?SERVICE_SUSPEND_KEY, AccountJObj)).

maybe_send_service_suspend_update(AccountId, AccountJObj, 'true') ->
    case key_tstamp(?SERVICE_SUSPEND_KEY, AccountJObj) of
        ServiceSuspendSent when is_number(ServiceSuspendSent) ->
            Cycle = ?KEY_REPEAT(?SERVICE_SUSPEND_KEY),
            Diff = kz_time:current_tstamp() - ServiceSuspendSent,
            case Diff >= Cycle of
               'true' ->
                   'ok' = send_account_update(AccountId, ?SERVICE_SUSPENDED_TEMPLATE, customer_update_databag(AccountId)),
                   update_account_key_sent(?SERVICE_SUSPEND_KEY, AccountJObj);
               'false' ->
                   lager:debug("service suspended alert sent ~w seconds ago, repeats every ~w", [Diff, Cycle])
            end;
        _Else ->
            'ok' = send_account_update(AccountId, ?SERVICE_SUSPENDED_TEMPLATE, customer_update_databag(AccountId)),
            update_account_key_sent(?SERVICE_SUSPEND_KEY, AccountJObj)
    end;
maybe_send_service_suspend_update(AccountId, _AccountJObj, 'false') ->
    lager:debug("service suspended for account: ~p", [AccountId]).

-spec maybe_send_trial_has_expired_update(kz_term:ne_binary()) -> any().
maybe_send_trial_has_expired_update(AccountId) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    case key_tstamp(?TRIAL_HAS_EXPIRED_KEY, AccountJObj) of
        ServiceSuspendSent when is_number(ServiceSuspendSent) ->
            lager:debug("trial has expired update already sent for expired trial account: ~p", [AccountId]);
        _Else ->
            'ok' = send_account_update(AccountId, ?TRIAL_HAS_EXPIRED_TEMPLATE, customer_update_databag(AccountId)),
            update_account_key_sent(?TRIAL_HAS_EXPIRED_KEY, AccountJObj)
    end.

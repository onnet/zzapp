-module(onbill_notifications).

-export([send_account_update/3
        ,init/0
        ,maybe_send_account_updates/2
        ,mrc_approaching_sent/1, set_mrc_approaching_sent/1, reset_mrc_approaching_sent/1
        ,mrc_approaching_enabled/1, set_mrc_approaching_enabled/1, reset_mrc_approaching_enabled/1
        ,mrc_approaching_tstamp/1, set_mrc_approaching_tstamp/1, remove_mrc_approaching_tstamp/1
        ,mrc_approaching_enabled_exists/1
        ]).

-include("onbill.hrl").

-define(ONBILL_NOTIFICATION_ENABLED, <<"onbill_notification_enabled">>).
-define(MRC_APPROACHING_TEMPLATE, <<"customer_update_billing_period">>).
-define(MRC_APPROACHING_SENT, [<<"notifications">>, <<"mrc_approaching">>, <<"sent_mrc_approaching">>]).
-define(MRC_APPROACHING_ENABLED, [<<"notifications">>, <<"mrc_approaching">>, <<"enabled">>]).
-define(MRC_APPROACHING_TSTAMP, [<<"notifications">>, <<"mrc_approaching">>, <<"last_notification">>]).
-define(MRC_APPROACHING_PERIOD,
        kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<"mrc_approaching_period_days">>, 5)).
-define(MRC_APPROACHING_REPEAT,
        kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<"mrc_approaching_repeat_s">>, 1 * ?SECONDS_IN_DAY)).

-define(MACRO_VALUE(Key, Label, Name, Description)
       ,{Key
        ,kz_json:from_list([{<<"i18n_label">>, Label}
                           ,{<<"friendly_name">>, Name}
                           ,{<<"description">>, Description}
                           ])
        }).

-define(TEXT_PLAIN, <<"text/plain">>).
-define(TEXT_HTML, <<"text/html">>).

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

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CRAWLER)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CRAWLER)).

-spec init() -> 'ok'.
init() ->
    mrc_approaching_init().

-spec mrc_approaching_init() -> 'ok'.
mrc_approaching_init() ->
    teletype_templates:init(?MRC_APPROACHING_TEMPLATE, [{'macros', ?TEMPLATE_MACROS}
                                                       ,{'subject', <<"New billing period">> }
                                                       ,{'category', <<"user">>}
                                                       ,{'friendly_name', <<"New billing period">>}
                                                       ,{'to', ?TEMPLATE_TO}
                                                       ,{'from', ?TEMPLATE_FROM}
                                                       ,{'cc', ?TEMPLATE_CC}
                                                       ,{'bcc', ?TEMPLATE_BCC}
                                                       ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                                       ]),
    maybe_upload_attachments(?MRC_APPROACHING_TEMPLATE).



-spec send_account_update(ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
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

-spec build_customer_update_payload(ne_binary(), ne_binary(), kz_json:object()) -> kz_proplist().
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
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec maybe_send_account_updates(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_send_account_updates(AccountId, AccountJObj) ->
    case onbill_util:maybe_allow_postpay(AccountId) of
        'false' ->
            maybe_new_billing_period_approaching(AccountId, AccountJObj);
        _ -> 'ok'
    end.

-spec maybe_new_billing_period_approaching(ne_binary(), kz_account:doc()) -> 'ok'.
maybe_new_billing_period_approaching(AccountId, AccountJObj) ->
    Timestamp = kz_time:current_tstamp(),
    Days_Before_MRC_Update = ?MRC_APPROACHING_PERIOD,
    {StartYear, StartMonth, StartDay} = onbill_util:period_start_date(AccountId, Timestamp),
    case onbill_util:days_left_in_period(StartYear, StartMonth, StartDay, Timestamp) of
        DaysLeft when DaysLeft < Days_Before_MRC_Update ->
            case onbill_bk_util:current_usage_amount_in_units(AccountId)
                > wht_util:current_balance(AccountId)
            of
                'true' ->
                    maybe_send_new_billing_period_approaching_update(AccountId, AccountJObj, mrc_approaching_enabled(AccountJObj));
                _ ->
                    'ok'
            end;
        _ -> 'ok'
    end.

-spec maybe_send_new_billing_period_approaching_update(ne_binary(), kz_account:doc(), boolean()) -> 'ok'.
maybe_send_new_billing_period_approaching_update(AccountId, AccountJObj, 'true') ->
    case mrc_approaching_tstamp(AccountJObj) of
        MRC_ApproachingSent when is_number(MRC_ApproachingSent) ->
            Cycle = ?MRC_APPROACHING_REPEAT,
            Diff = kz_time:current_tstamp() - MRC_ApproachingSent,
            case Diff >= Cycle of
               'true' ->
                   'ok' = send_account_update(AccountId, ?MRC_APPROACHING_TEMPLATE, mrc_approaching_databag(AccountId)),
                   update_account_mrc_approaching_sent(AccountJObj);
               'false' ->
                   lager:debug("mrc approaching alert sent ~w seconds ago, repeats every ~w", [Diff, Cycle])
            end;
        _Else ->
            'ok' = send_account_update(AccountId, ?MRC_APPROACHING_TEMPLATE, mrc_approaching_databag(AccountId)),
            update_account_mrc_approaching_sent(AccountJObj)
    end,
    'ok';
maybe_send_new_billing_period_approaching_update(AccountId, _AccountJObj, 'false') ->
    lager:debug("mrc approaching alert disabled for Account: ~p", [AccountId]).

-spec mrc_approaching_databag(ne_binary()) -> kz_json:object().
mrc_approaching_databag(AccountId) ->
    Values = [{<<"reseller">>, reseller_info_databag(AccountId)}
             ,{<<"account">>, account_info_databag(AccountId)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec reseller_info_databag(ne_binary()) -> kz_json:object().
reseller_info_databag(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    {'ok', ResellerDoc} = kz_account:fetch(ResellerId),
    Values = [{<<"name">>, kz_account:name(ResellerDoc)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec account_info_databag(ne_binary()) -> kz_json:object().
account_info_databag(AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    Values = [{<<"name">>, kz_account:name(AccountDoc)}
             ],
    kz_json:set_values(Values, kz_json:new()).

-spec update_account_mrc_approaching_sent(kz_account:doc()) -> 'ok'.
update_account_mrc_approaching_sent(AccountJObj0) ->
    AccountJObj1 = set_mrc_approaching_sent(AccountJObj0),
    AccountJObj2 = set_mrc_approaching_tstamp(AccountJObj1),
    _ = kz_util:account_update(AccountJObj2),
   'ok'.

-spec onbill_notification_enabled(ne_binary()) -> boolean().
onbill_notification_enabled(AccountId) ->
    kz_json:is_true(?ONBILL_NOTIFICATION_ENABLED, onbill_util:reseller_vars(AccountId)).

-spec mrc_approaching_sent(kz_account:doc()) -> boolean().
mrc_approaching_sent(JObj) ->
    kz_json:is_true(?MRC_APPROACHING_SENT, JObj).

-spec set_mrc_approaching_sent(kz_account:doc()) -> kz_account:doc().
set_mrc_approaching_sent(JObj) ->
    kz_json:set_value(?MRC_APPROACHING_SENT, 'true', JObj).

-spec reset_mrc_approaching_sent(kz_account:doc()) -> kz_account:doc().
reset_mrc_approaching_sent(JObj) ->
    kz_json:set_value(?MRC_APPROACHING_SENT, 'false', JObj).

-spec mrc_approaching_enabled(kz_account:doc()) -> boolean().
mrc_approaching_enabled(JObj) ->
    kz_json:is_true(?MRC_APPROACHING_ENABLED, JObj, 'true').

-spec set_mrc_approaching_enabled(kz_account:doc()) -> kz_account:doc().
set_mrc_approaching_enabled(JObj) ->
    kz_json:set_value(?MRC_APPROACHING_ENABLED, 'true', JObj).

-spec reset_mrc_approaching_enabled(kz_account:doc()) -> kz_account:doc().
reset_mrc_approaching_enabled(JObj) ->
    kz_json:set_value(?MRC_APPROACHING_ENABLED, 'false', JObj).

-spec mrc_approaching_enabled_exists(kz_account:doc()) -> boolean().
mrc_approaching_enabled_exists(JObj) ->
    kz_json:get_ne_value(?MRC_APPROACHING_ENABLED, JObj) =/= 'undefined'.

-spec mrc_approaching_tstamp(kz_account:doc()) -> api_number().
mrc_approaching_tstamp(JObj) ->
    kz_json:get_integer_value(?MRC_APPROACHING_TSTAMP, JObj).

-spec set_mrc_approaching_tstamp(kz_account:doc()) -> kz_account:doc().
set_mrc_approaching_tstamp(JObj) ->
    TStamp = kz_time:current_tstamp(),
    set_mrc_approaching_tstamp(JObj, TStamp).

-spec set_mrc_approaching_tstamp(kz_account:doc(), number()) -> kz_account:doc().
set_mrc_approaching_tstamp(JObj, TStamp) ->
    kz_json:set_value(?MRC_APPROACHING_TSTAMP, TStamp, JObj).

-spec remove_mrc_approaching_tstamp(kz_account:doc()) -> kz_account:doc().
remove_mrc_approaching_tstamp(JObj) ->
    kz_json:delete_key(?MRC_APPROACHING_TSTAMP, JObj).

maybe_upload_attachments(TemplateId) ->
    Id = kz_notification:db_id(TemplateId),
    case does_attachment_exist(Id, attachment_name(?TEXT_HTML)) of
        'true' -> 'ok';
        'false' ->
            case read_template_from_disk(TemplateId, 'html') of
                {'ok', HTML} ->
                    upload_attachment(HTML, ?TEXT_HTML, Id, attachment_name(?TEXT_HTML));
                {'error', _EHTML} ->
                    lager:info("failed to find template '~s.html': ~p", [TemplateId, _EHTML])
            end
    end,
    case does_attachment_exist(Id, attachment_name(?TEXT_PLAIN)) of
        'true' -> 'ok';
        'false' ->
            case read_template_from_disk(TemplateId, 'text') of
                {'ok', Text} ->
                    upload_attachment(Text, ?TEXT_PLAIN, Id, attachment_name(?TEXT_PLAIN));
                {'error', _ETEXT} ->
                    lager:info("failed to find template '~s.text': ~p", [TemplateId, _ETEXT])
            end
    end.

-spec does_attachment_exist(ne_binary(), ne_binary()) -> boolean().
does_attachment_exist(DocId, AName) ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId) of
        {'ok', JObj} ->
            kz_doc:attachment(JObj, cow_qs:urldecode(AName)) =/= 'undefined';
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.

-spec read_template_from_disk(ne_binary(), 'html' | 'text') ->
                {'ok', binary()} |
                {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
read_template_from_disk(TemplateId, Type) ->
    File = template_filename(TemplateId, Type),
    file:read_file(File).

upload_attachment(Contents, ContentType, Id, AName) ->
    case save_attachment(Id, AName, ContentType, Contents) of
        {'ok', AttachmentJObj} ->
            lager:debug("saved attachment: ~p", [AttachmentJObj]);
        {'error', _E} ->
            lager:debug("failed to save attachment ~s: ~p", [AName, _E])
    end.

-spec attachment_name(ne_binary()) -> ne_binary().
attachment_name(ContentType) ->
    kz_binary:clean(<<"template.", (kz_http_util:urlencode(ContentType))/binary>>).

-spec template_filename(ne_binary(), 'html' | 'text') -> file:filename_all().
template_filename(TemplateId, Type) ->
    Basename = iolist_to_binary([TemplateId, ".", kz_term:to_list(Type)]),
    filename:join([code:priv_dir('onbill')
                  ,"templates"
                  ,Basename
                  ]).

-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), binary()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
save_attachment(DocId, AName, ContentType, Contents) ->
    case
        kz_datamgr:put_attachment(
          ?KZ_CONFIG_DB
                                 ,DocId
                                 ,AName
                                 ,Contents
                                 ,[{'content_type', kz_term:to_list(ContentType)}]
         )
    of
        {'ok', _UpdatedJObj}=OK ->
            lager:debug("added attachment ~s to ~s", [AName, DocId]),
            OK;
        {'error', 'conflict'}=E ->
            case does_attachment_exist(DocId, AName) of
                'true' ->
                    lager:debug("added attachment ~s to ~s", [AName, DocId]),
                    kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId);
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    E
            end;
        {'error', _E}=E ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            E
    end.


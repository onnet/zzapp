-module(onbill_notifications).

-export([send_account_update/1
        ,maybe_send_account_updates/1
        ]).

-include("onbill.hrl").

-define(KEY_TRIAL_EXPIRATION, <<"pvt_trial_expires">>).

-spec send_account_update(ne_binary()) -> 'ok'.
send_account_update(AccountId) ->
    case kz_amqp_worker:call(build_customer_update_payload(AccountId)
                            ,fun kapi_notifications:publish_customer_update/1
                            ,fun kapi_notifications:customer_update_v/1
                            )
    of
        {'ok', _Resp} ->
            lager:debug("published customer_update notification");
        {'error', _E} ->
            lager:debug("failed to publish_customer update notification: ~p", [_E])
    end.

-spec build_customer_update_payload(cb_context:context()) -> kz_proplist().
build_customer_update_payload(AccountId) ->
    props:filter_empty(
      [{<<"Account-ID">>, kz_services:find_reseller_id(AccountId)}
      ,{<<"Recipient-ID">>, AccountId}
   %   ,{<<"User-Type">>, <<"admins_only">>}
   %   ,{<<"Subject">>, <<"OnBill test subject">>}
   %   ,{<<"From">>, <<"crm@onnet.su">>}
      ,{<<"Template-ID">>, <<"customer_update_billing_period">>}
      ,{<<"DataBag">>, {[{<<"field1">>,<<"value1">>},{<<"field2">>,{[{<<"subfield1">>,<<"subvalue1">>},{<<"subfield2">>,<<"subvalue2">>}]}}]}}
   %   ,{<<"Reply-To">>, <<"iam@onnet.info">>}
      ,{<<"HTML">>, base64:encode(<<"Dear {{user.first_name}} {{user.last_name}}. <br /><br />DataBag test: {{databag.field2.subfield1}} <br /><br /> Kind regards,">>)}
      ,{<<"Text">>, <<"Oh Dear {{user.first_name}} {{user.last_name}}.\n\nDataBag test: {{databag.field2.subfield2}}\n\nBest regards,">>}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec maybe_send_account_updates(ne_binary()) -> 'ok'.
maybe_send_account_updates(AccountId) ->
   maybe_new_billing_period_approaching(AccountId).

-spec maybe_new_billing_period_approaching(ne_binary()) -> 'ok'.
maybe_new_billing_period_approaching(AccountId) ->
    Timestamp = kz_time:current_tstamp(),
    {StartYear, StartMonth, StartDay} = onbill_util:period_start_date(AccountId, Timestamp),
    case onbill_util:days_left_in_period(StartYear, StartMonth, StartDay, Timestamp) of
        DaysLeft when DaysLeft > 5 ->
            maybe_send_new_billing_period_approaching_update(AccountId);
        _ -> 'ok'
    end.

-spec maybe_send_new_billing_period_approaching_update(ne_binary()) -> 'ok'.
maybe_send_new_billing_period_approaching_update(_AccountId) ->
    'ok'.


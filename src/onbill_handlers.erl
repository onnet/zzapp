%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(onbill_handlers).

-export([handle_doc_created/2
        ,handle_doc_edited/2
        ,handle_logger/2
        ]).

-include("onbill.hrl").

-spec handle_doc_created(kz_json:object(), kz_proplist()) -> any().
handle_doc_created(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
         %    andalso not kz_services:is_reseller(AccountId)
             andalso not kapps_util:is_master_account(AccountId)
    of
        'true' ->
            handle_doc_created(kz_json:get_value(<<"Type">>, JObj), AccountId, JObj);
        'false' ->
            'ok'
    end.

handle_doc_created(_, 'undefined', _) ->
    'ok';
handle_doc_created(<<"device">>, AccountId, _JObj) ->
    lager:info("IAM handle_doc_created before maybe_reconcile Account: ~p",[AccountId]),
    _ = onbill_util:maybe_reconcile(AccountId);
handle_doc_created(<<"user">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(<<"number">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(<<"limits">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(<<"credit">>, AccountId, _JObj) ->
    _ = onbill_util:ensure_service_plan(AccountId),
    _ = kz_services:reconcile(AccountId),
    kz_service_sync:sync(AccountId);
handle_doc_created(_, _, _) ->
    'ok'.

-spec handle_doc_edited(kz_json:object(), kz_proplist()) -> any().
handle_doc_edited(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
         %    andalso not kz_services:is_reseller(AccountId)
             andalso not kapps_util:is_master_account(AccountId)
    of
        'true' ->
            handle_doc_edited(kz_json:get_value(<<"Type">>, JObj), AccountId, JObj);
        'false' ->
            'ok'
    end.

handle_doc_edited(_, 'undefined', _) ->
    'ok';
handle_doc_edited(<<"limits">>, AccountId, _) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_edited(<<"service">>, AccountId, _) ->
    _ = onbill_util:ensure_service_plan(AccountId),
    Services = kz_services:fetch(AccountId),
    case kz_services:is_dirty(Services) of
        'true' ->
            lager:info("IAM handle_doc_edited dirty Account: ~p",[AccountId]),
            _ = kz_service_sync:sync(AccountId);
        'false' ->
            'ok'
    end;
handle_doc_edited(_, _, _) ->
    'ok'.

-spec handle_logger(kz_json:object(), kz_proplist()) -> any().
handle_logger(JObj, _Props) ->
    lager:info("IAM listen to JObj: ~p",[JObj]).

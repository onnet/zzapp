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
        ,handle_bookkeepers/2
        ,handle_logger/2
        ]).

-include("onbill.hrl").

-spec handle_doc_created(kz_json:object(), kz_term:proplist()) -> any().
handle_doc_created(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case (AccountId /= 'undefined')
         andalso kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
         andalso not kapps_util:is_master_account(AccountId)
    of
        'true' ->
            handle_doc_created(kz_json:get_value(<<"Type">>, JObj), AccountId, JObj);
        'false' ->
            'ok'
    end.

handle_doc_created(<<"account">>, AccountId, _JObj) ->
    _ = timer:sleep(2000),
    _ = zz_util:ensure_service_plan(AccountId);
handle_doc_created(<<"device">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId),
    _ = kz_services_bookkeeper:sync(AccountId);
handle_doc_created(<<"user">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId),
    _ = kz_services_bookkeeper:sync(AccountId);
handle_doc_created(<<"number">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId),
    _ = kz_services_bookkeeper:sync(AccountId);
handle_doc_created(<<"limits">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId),
    _ = kz_services_bookkeeper:sync(AccountId);
handle_doc_created(<<"credit">>, AccountId, _JObj) ->
    _ = zz_util:ensure_service_plan(AccountId),
    _ = kz_services:reconcile(AccountId),
    _ = kz_services_bookkeeper:sync(AccountId);
handle_doc_created(_, _, _) ->
    'ok'.

-spec handle_doc_edited(kz_json:object(), kz_term:proplist()) -> any().
handle_doc_edited(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case (AccountId /= 'undefined')
         andalso kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
         andalso not kapps_util:is_master_account(AccountId)
    of
        'true' ->
            handle_doc_edited(kz_json:get_value(<<"Type">>, JObj), AccountId, JObj);
        'false' ->
            'ok'
    end.

handle_doc_edited(<<"limits">>, AccountId, _) ->
    _ = zz_util:reconcile_and_maybe_sync(AccountId),
    _ = timer:sleep(2000),
    _ = zz_util:reconcile_and_maybe_sync(AccountId);
handle_doc_edited(_, _, _) ->
    'ok'.

-spec handle_bookkeepers(kz_json:object(), kz_term:proplist()) -> any().
handle_bookkeepers(JObj, Props) ->
    lager:info("IAM handle_bookkeepers listen to Props: ~p",[Props]),
    lager:info("IAM handle_bookkeepers Bookkeeper-Type: ~p",[kz_json:get_value(<<"Bookkeeper-Type">>, JObj)]),
    lager:info("IAM handle_bookkeepers Items: ~p",[kz_json:get_value(<<"Items">>, JObj, [])]),
  Items = kz_json:get_value(<<"Items">>, JObj, []),
[lager:info("IAM handle_bookkeepers Item by Item: ~p",[Item]) || Item <- Items],
    lager:info("IAM handle_bookkeepers Items kz_json:is_json_object: ~p",[kz_json:is_json_object(Items)]),
    lager:info("IAM handle_bookkeepers Vendor-ID: ~p",[kz_json:get_value(<<"Vendor-ID">>, JObj)]),
    lager:info("IAM handle_bookkeepers Bookkeeper-ID: ~p",[kz_json:get_value(<<"Bookkeeper-ID">>, JObj)]),
    lager:info("IAM handle_bookkeepers listen to JObj: ~p",[JObj]).

-spec handle_logger(kz_json:object(), kz_term:proplist()) -> any().
handle_logger(_JObj, _Props) ->
  %  lager:info("IAM listen to Props: ~p",[Props]),
  %  lager:info("IAM listen to JObj: ~p",[JObj]),
    ok.

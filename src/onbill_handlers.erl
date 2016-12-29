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
    handle_doc_created(kz_json:get_value(<<"Type">>, JObj)
                      ,kz_json:get_value(<<"Account-ID">>, JObj)
                      ,JObj 
                      ).

handle_doc_created(_, 'undefined', _) ->
    'ok';
handle_doc_created(<<"device">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(<<"user">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(<<"number">>, AccountId, _JObj) ->
    _ = kz_services:reconcile(AccountId);
handle_doc_created(_, _, _) ->
    'ok'.

-spec handle_doc_edited(kz_json:object(), kz_proplist()) -> any().
handle_doc_edited(JObj, _Props) ->
    handle_doc_edited(kz_json:get_value(<<"Type">>, JObj)
                     ,kz_json:get_value(<<"Account-ID">>, JObj)
                     ,JObj 
                     ).

handle_doc_edited(_, 'undefined', _) ->
    'ok';
handle_doc_edited(<<"service">>, AccountId, _) ->
    case kz_datamgr:open_doc(?SERVICES_DB, AccountId) of
        {'ok', ServiceDoc} ->
            case kz_json:get_value(<<"pvt_dirty">>, ServiceDoc, 'false') of
                'true' -> kz_service_sync:sync(AccountId);
                'false' -> 'ok'
            end;
        _ ->
            lager:info("can't open services doc ~p in services db, please check if it exists",[AccountId])
    end;
handle_doc_edited(_, _, _) ->
    'ok'.

-spec handle_logger(kz_json:object(), kz_proplist()) -> any().
handle_logger(JObj, _Props) ->
    lager:info("IAM listen to JObj: ~p",[JObj]),
    ok.

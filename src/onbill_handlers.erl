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
        ,handle_logger/2
        ]).

-include("onbill.hrl").

-spec handle_doc_created(kz_json:object(), kz_proplist()) -> any().
handle_doc_created(JObj, _Props) ->
    lager:info("IAM listen doc_dreated JObj: ~p",[JObj]),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    case kz_json:get_value(<<"Type">>, JObj) of
        <<"device">> ->
            _ = kz_services:reconcile(AccountId),
            _ = kz_service_sync:sync(AccountId);
        <<"user">> ->
            _ = kz_services:reconcile(AccountId),
            _ = kz_service_sync:sync(AccountId);
        _ ->
            'ok'
    end.

-spec handle_logger(kz_json:object(), kz_proplist()) -> any().
handle_logger(JObj, _Props) ->
    lager:info("IAM listen to JObj: ~p",[JObj]),
 %%   lager:info("IAM listen to Props: ~p",[Props]),
    ok.

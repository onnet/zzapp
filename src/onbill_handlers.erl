%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(onbill_handlers).

-export([handle_logger/2
        ]).

-include("onbill.hrl").

-spec handle_logger(kz_json:object(), kz_proplist()) -> any().
handle_logger(_JObj, _Props) ->
%%    lager:info("IAM listen to JObj: ~p",[JObj]),
%%    lager:info("IAM listen to Props: ~p",[Props]).
    ok.

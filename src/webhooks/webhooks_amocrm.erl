%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_amocrm).

-export([init/0
        ,bindings_and_responders/0
        ,handle/2
        ]).

-include_lib("webhooks/src/webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"amocrm">>).
-define(NAME, <<"amocrm">>).
-define(DESC, <<"This webhook notifies AmoCRM about call events">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ])
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {[{'call', [{'restrict_to', ['CHANNEL_CREATE', 'CHANNEL_ANSWER', 'CHANNEL_DESTROY']}
               ]
      }
     ]
    ,[{{?MODULE, 'handle'} %,[{<<"*">>,<<"*">>}]
      ,[{<<"call_event">>, <<"CHANNEL_CREATE">>}
       ,{<<"call_event">>, <<"CHANNEL_ANSWER">>}
       ,{<<"call_event">>, <<"CHANNEL_DESTROY">>}
       ]
      }
     ]
    }.

-spec handle(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle(JObj, _Props) ->
  lager:info("IAM webhook JObj: ~p",[JObj]),
  lager:info("IAM webhook _Props: ~p",[_Props]),
    'true' = kapi_call:event_v(JObj),
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
  lager:info("IAM webhook AccountId: ~p",[AccountId]),
    maybe_send_event(AccountId, format(JObj)).

-spec maybe_send_event(kz_term:api_binary(), kz_json:object()) -> 'ok'.
maybe_send_event('undefined', _JObj) -> 'ok';
maybe_send_event(AccountId, JObj) ->
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] -> lager:debug("no hooks to handle for ~s", [AccountId]);
        Hooks -> webhooks_util:fire_hooks(JObj, Hooks)
    end.

-spec format(kz_json:object()) -> kz_json:object().
format(JObj) ->
    AccountId = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    JObj1 = kz_json:set_value(<<"Account-ID">>, AccountId, JObj),
    RemoveKeys = [<<"Node">>
                 ,<<"Msg-ID">>
                 ,<<"App-Version">>
                 ,<<"App-Name">>
                 ,<<"Event-Category">>
                 ,<<"Custom-Channel-Vars">>
                 ],
    kz_json:normalize_jobj(JObj1, RemoveKeys, []).

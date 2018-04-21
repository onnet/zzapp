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
    ,[{{?MODULE, 'handle'}
      ,[{<<"call_event">>, <<"CHANNEL_CREATE">>}
       ,{<<"call_event">>, <<"CHANNEL_ANSWER">>}
       ,{<<"call_event">>, <<"CHANNEL_DESTROY">>}
       ]
      }
     ]
    }.

-spec handle(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    maybe_handle_event(AccountId, format(JObj)).

-spec maybe_handle_event(kz_term:api_binary(), kz_json:object()) -> 'ok'.
maybe_handle_event('undefined', _JObj) -> 'ok';
maybe_handle_event(AccountId, JObj) ->
  lager:info("IAM webhook JObj: ~p",[JObj]),
  lager:info("IAM webhook AccountId: ~p",[AccountId]),
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle for ~s", [AccountId]);
        Hooks ->
            filter_hooks(JObj, Hooks)
    end.

handle_event(<<"CHANNEL_CREATE">>, JObj, Hook) ->
    lager:info("=================================================="),
    lager:info("IAM print_hooks event_name: CHANNEL_CREATE"),
    lager:info("IAM print_hooks webhook.hook_event: ~p", [Hook#webhook.hook_event]),
    lager:info("=================================================="),
    {'ok', Cookie} = amocrm_auth_cookie(Hook),
    contact_lookup(Cookie, JObj, Hook),
    'ok';

handle_event(<<"CHANNEL_ANSWER">>, JObj, Hook) ->
    lager:info("=================================================="),
    lager:info("IAM print_hooks event_name: CHANNEL_ANSWER"),
    lager:info("IAM print_hooks webhook.hook_event: ~p", [Hook#webhook.hook_event]),
    lager:info("=================================================="),
    {'ok', Cookie} = amocrm_auth_cookie(Hook),
    case contact_lookup(Cookie, JObj, Hook) of
        {'ok', Contact} ->
        {'ok', 'contact_not_found'} ->
        {'error', 'contact_lookup_failed'} ->
            'ok'
    end;

handle_event(<<"CHANNEL_DESTROY">>, _JObj, Hook) ->
    lager:info("=================================================="),
    lager:info("IAM print_hooks event_name: CHANNEL_DESTROY"),
    lager:info("IAM print_hooks webhook.hook_event: ~p", [Hook#webhook.hook_event]),
    lager:info("=================================================="),
    'ok'.

compose_note(JObj, Contact) ->
    ElementTypes = [{<<"contact">>, 1}
                   ,{<<"lead">>, 2}
                   ,{<<"company">>, 3}
                   ,{<<"task">>, 4}
                   ],
    Values = [{<<"element_id">>, kz_json:get_value(<<"id">>, Contact)}
             ,{<<"element_type">>, kz_json:get_value(kz_json:get_value(<<"type">>, Contact), ElementTypes, 1)}
             ],

filter_hooks(_, []) -> 'ok';
filter_hooks(JObj, [#webhook{hook_event = <<"amocrm">>}=Hook|T]) ->
    handle_event(kz_json:get_value(<<"event_name">>, JObj), JObj, Hook),
    filter_hooks(JObj, T);
filter_hooks(JObj, [_|T]) ->
    filter_hooks(JObj, T).

-spec format(kz_json:object()) -> kz_json:object().
format(JObj) ->
    AccountId = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    JObj1 = kz_json:set_value(<<"Account-ID">>, AccountId, JObj),
    RemoveKeys = [<<"Node">>
                 ,<<"Msg-ID">>
                 ,<<"App-Version">>
                 ,<<"App-Name">>
                 ,<<"Event-Category">>
              %   ,<<"Custom-Channel-Vars">>
                 ],
    kz_json:normalize_jobj(JObj1, RemoveKeys, []).

amocrm_auth_cookie(#webhook{uri = URL
                           ,custom_data = CustomData
                           }
                  ) ->
    UserLogin = kz_json:get_first_defined([<<"USER_LOGIN">>, <<"user_login">>], CustomData),
    UserHash = kz_json:get_first_defined([<<"USER_HASH">>, <<"user_hash">>], CustomData),
    case kz_http:post(<<URL/binary, "/private/api/auth.php?type=json">>
                     ,[]
                     ,{[{<<"USER_LOGIN">>,UserLogin},{<<"USER_HASH">>,UserHash}]})
    of
        {'ok', 200, Headers, _Body} ->
            lager:info("IAM amocrm_auth: Headers, ~p",[Headers]),
            Cookie = props:get_value("set-cookie", Headers),
            lager:info("IAM amocrm_auth: Cookie, ~p",[Cookie]),
            {'ok', Cookie};
        {_, Code, Headers, Body} ->
            lager:info("IAM amocrm_auth Failed Code: ~p",[Code]),
            lager:info("IAM amocrm_auth Failed Headers: ~p",[Headers]),
            lager:info("IAM amocrm_auth Failed Body: ~p",[Body]),
            {'error', 'auth_failed'}
    end.

caller_number(JObj) ->
    kz_json:get_first_defined([<<"caller_id_number">>], JObj).

contact_lookup(Cookie, JObj, #webhook{uri = URL}) ->
    CallerNumber = caller_number(JObj),
    lager:info("IAM contact_lookup CallerNumber: ~p",[CallerNumber]),
    case kz_http:get(<<URL/binary, "/private/api/v2/json/contacts/list?query=", CallerNumber/binary>>
                    ,[{<<"Cookie">>, Cookie}]
                    )
    of
        {'ok', 200, _Headers, Body} ->
            lager:info("IAM contact_lookup successful Body: ~p",[kz_json:decode(Body)]),
            [Contact|_] = kz_json:get_value([<<"response">>,<<"contacts">>], kz_json:decode(Body)),
            lager:info("IAM contact_lookup successful Contact: ~p",[Contact]),
            {'ok', Contact};
        {'ok', 204, _Headers, _Body} ->
            lager:info("IAM contact_lookup returned empty result."),
            {'ok', 'contact_not_found'};
        {_, Code, Headers, Body} ->
            lager:info("IAM contact_lookup Failed Code: ~p",[Code]),
            lager:info("IAM contact_lookup Failed Headers: ~p",[Headers]),
            lager:info("IAM contact_lookup Failed Body: ~p",[Body]),
            {'error', 'contact_lookup_failed'}
    end.

set_note(Cookie, DataBag, #webhook{uri = URL}) ->
    case kz_http:post(<<URL/binary, "/private/api/auth.php?type=json">>
                     ,[{<<"Cookie">>, Cookie}]
                     ,DataBag
                     )
    of
        {'ok', 200, _Headers, Body} ->
            lager:info("IAM set_note successful Body: ~p",[kz_json:decode(Body)]),
            ok;
        {_, Code, Headers, Body} ->
            lager:info("IAM set_note Failed Code: ~p",[Code]),
            lager:info("IAM set_note Failed Headers: ~p",[Headers]),
            lager:info("IAM set_note Failed Body: ~p",[Body]),
            {'error', 'set_note_failed'}
    end.

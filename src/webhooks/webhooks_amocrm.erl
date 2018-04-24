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
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle for ~s", [AccountId]);
        Hooks ->
            filter_hooks(JObj, Hooks)
    end.

handle_event(<<"CHANNEL_CREATE">>, JObj, Hook) ->
    lager:info("IAM ~p handle_event CHANNEL_CREATE JObj: ~p", [Hook#webhook.hook_event, JObj]),
    'ok';

handle_event(<<"CHANNEL_ANSWER">>, JObj, Hook) ->
    lager:info("IAM ~p handle_event CHANNEL_ANSWER JObj: ~p", [Hook#webhook.hook_event, JObj]),
    {'ok', Cookie, _AccountId, _UserId} = amocrm_auth(Hook),
    case contact_lookup(Cookie, JObj, Hook) of
        {'ok', 'contact_not_found'} ->
            'ok';
        {'ok', Contact} ->
            DataBag = kz_json:set_value(<<"add">>, [compose_note(JObj, Contact)], kz_json:new()),
            set_note(Cookie, DataBag, Hook);
        {'error', 'contact_lookup_failed'} ->
            'ok'
    end;

handle_event(<<"CHANNEL_DESTROY">>, JObj, Hook) ->
    lager:info("IAM ~p handle_event CHANNEL_DESTROY JObj: ~p", [Hook#webhook.hook_event, JObj]),
    {'ok', Cookie, AccountId, _UserId} = amocrm_auth(Hook),
    case contact_lookup(Cookie, JObj, Hook) of
        {'ok', 'contact_not_found'} ->
            'ok';
        {'ok', Contact} ->
            DataBag = kz_json:set_value(<<"add">>, [compose_note(JObj, Contact)], kz_json:new()),
            set_note(Cookie, DataBag, Hook),
          Res2 =  call_add(AccountId, JObj),
            lager:info("IAM print_hooks call_add Res2: ~p", [Res2]),
            'ok';
        {'error', 'contact_lookup_failed'} ->
            'ok'
    end.

filter_hooks(_, []) -> 'ok';
filter_hooks(JObj, [#webhook{hook_event = <<"amocrm">>}=Hook|T]) ->
    handle_event(kz_json:get_first_defined([<<"event_name">>], JObj), JObj, Hook),
    filter_hooks(JObj, T);
filter_hooks(JObj, [_|T]) ->
    filter_hooks(JObj, T).

amocrm_auth(#webhook{uri = URL
                           ,custom_data = CustomData
                           }
                  ) ->
    UserLogin = kz_json:get_first_defined([<<"USER_LOGIN">>, <<"user_login">>], CustomData),
    UserHash = kz_json:get_first_defined([<<"USER_HASH">>, <<"user_hash">>], CustomData),
    case kz_http:post(<<URL/binary, "/private/api/auth.php?type=json">>
                     ,[]
                     ,{[{<<"USER_LOGIN">>,UserLogin},{<<"USER_HASH">>,UserHash}]})
    of
        {'ok', 200, Headers, Body} ->
            lager:info("IAM amocrm_auth: Headers, ~p",[Headers]),
            lager:info("IAM amocrm_auth: Body, ~p",[Body]),
            DecodedBody = kz_json:decode(Body),
            lager:info("IAM amocrm_auth: decoded Body, ~p",[DecodedBody]),
            [Account|_] = kz_json:get_value([<<"response">>, <<"accounts">>], DecodedBody),
            AccountId = kz_json:get_value(<<"id">>, Account),
            UserId = kz_json:get_value([<<"response">>, <<"user">>, <<"id">>], DecodedBody),
            lager:info("IAM amocrm_auth: AccountId, ~p",[AccountId]),
            lager:info("IAM amocrm_auth: UserId, ~p",[UserId]),
            Cookie = props:get_value("set-cookie", Headers),
            lager:info("IAM amocrm_auth: Cookie, ~p",[Cookie]),
            {'ok', Cookie, AccountId, UserId};
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
    case kz_http:post(<<URL/binary, "/api/v2/notes">>
                     ,[{<<"Cookie">>, Cookie}]
                     ,DataBag
                     )
    of
        {'ok', 200, Headers, Body} ->
            lager:info("IAM set_note successful Headers: ~p",[Headers]),
            lager:info("IAM set_note successful Body: ~p",[kz_json:decode(Body)]),
            ok;
        {_, Code, Headers, Body} ->
            lager:info("IAM set_note Failed Code: ~p",[Code]),
            lager:info("IAM set_note Failed Headers: ~p",[Headers]),
            lager:info("IAM set_note Failed Body: ~p",[Body]),
            {'error', 'set_note_failed'}
    end.

compose_note(JObj, Contact) ->
    ElementTypes = [{<<"contact">>, 1}
                   ,{<<"lead">>, 2}
                   ,{<<"company">>, 3}
                   ,{<<"task">>, 4}
                   ],
    NoteTypes = [{<<"inbound">>, 10}
                ,{<<"outbound">>, 11}
                ],
    Values = props:filter_empty(
        [{<<"element_id">>, kz_json:get_value(<<"id">>, Contact)}
        ,{<<"element_type">>, props:get_value(kz_json:get_value(<<"type">>, Contact), ElementTypes, 1)}
        ,{<<"note_type">>, props:get_value(kz_json:get_value(<<"call_direction">>, JObj), NoteTypes, 0)}
        ,{<<"date_create">>, kz_time:gregorian_seconds_to_unix_seconds(kz_json:get_value(<<"timestamp">>, JObj))}
        ,{<<"request_id">>, kz_json:get_value(<<"call_id">>, JObj)}
        ,{[<<"params">>,<<"UNIQ">>], kz_json:get_value(<<"call_id">>, JObj)}
        ,{[<<"params">>,<<"PHONE">>], caller_number(JObj)}
        ,{[<<"params">>,<<"DURATION">>], kz_json:get_value(<<"billing_seconds">>, JObj)}
        ,{[<<"params">>,<<"SRC">>], <<"kzonnet">>}
        ]),
    kz_json:set_values(Values, kz_json:new()).


call_add(AmoAccountId, JObj) ->
    Code = <<"kzonnet">>,
    Key = <<"cf87ea38decf7141244997f1e3acd8e3edc42ad93f55c6150b425e5d776bf582">>,
    case kz_http:post(<<"https://sip.amocrm.ru/api/calls/add/"
                       ,"?code=", Code/binary
                       ,"&key=", Key/binary
                       ,"&account_id=", AmoAccountId/binary>>
                     ,[]
                     ,kz_json:set_value([<<"request">>,<<"add">>], [call_add_databag(JObj)], kz_json:new())
                     )
    of
        {'ok', 200, Headers, Body} ->
            lager:info("IAM call_add successful Headers: ~p",[Headers]),
            lager:info("IAM call_add successful Body: ~p",[kz_json:decode(Body)]),
            ok;
        {_, Code, Headers, Body} ->
            lager:info("IAM call_add Failed Code: ~p",[Code]),
            lager:info("IAM call_add Failed Headers: ~p",[Headers]),
            lager:info("IAM call_add Failed Body: ~p",[kz_json:decode(Body)]),
            {'error', 'call_add_failed'}
    end.

call_add_databag(JObj) ->
            lager:info("IAM call_add_databag JObj: ~p",[JObj]),
    Values =
        [{<<"uuid">>, kz_json:get_value(<<"call_id">>, JObj)}
        ,{<<"caller">>, 173}
        ,{<<"to">>, <<"79493786247">>}
        ,{<<"date">>, kz_time:gregorian_seconds_to_unix_seconds(kz_json:get_value(<<"timestamp">>, JObj))}
        ,{<<"billsec">>, 49}
        ,{<<"type">>, <<"inbound">>}
        ,{<<"link">>, <<"http://www.example.net/call_66cadef07c67a314389a824fd8fa0cd1.mp3">>}
        ],
    kz_json:set_values(Values, kz_json:new()).

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

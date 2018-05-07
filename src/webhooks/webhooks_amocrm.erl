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
            Note = compose_note(JObj, Contact),
            case note_lookup(kz_json:get_value(<<"call_id">>, JObj), list_notes(Cookie, Hook)) of
                'undefined' ->
                    DataBag = kz_json:set_value(<<"add">>, [Note], kz_json:new()),
                    set_note(Cookie, DataBag, Hook);
                NoteId ->
                    UpdValues =
                        [{<<"id">>, NoteId}
                        ,{<<"updated_at">>, kz_time:current_unix_tstamp()}], 
                    DataBag = kz_json:set_value(<<"update">>, [kz_json:set_values(UpdValues, Note)], kz_json:new()),
                    set_note(Cookie, DataBag, Hook)
            end;
        {'error', 'contact_lookup_failed'} ->
            'ok'
    end;

handle_event(<<"CHANNEL_DESTROY">>, JObj, Hook) ->
    lager:info("IAM ~p handle_event CHANNEL_DESTROY JObj: ~p", [Hook#webhook.hook_event, JObj]),
    {'ok', Cookie, AccountId, _UserId} = amocrm_auth(Hook),
    case contact_lookup(Cookie, JObj, Hook) of
        {'ok', 'contact_not_found'} ->
            call_add(AccountId, JObj),
            'ok';
        {'ok', Contact} ->
            Note = compose_note(JObj, Contact),
            case note_lookup(kz_json:get_value(<<"call_id">>, JObj), list_notes(Cookie, Hook)) of
                'undefined' ->
                    DataBag = kz_json:set_value(<<"add">>, [Note], kz_json:new()),
                    set_note(Cookie, DataBag, Hook);
                NoteId ->
                    UpdValues =
                        [{<<"id">>, NoteId}
                        ,{<<"updated_at">>, kz_time:current_unix_tstamp()}], 
                    DataBag = kz_json:set_value(<<"update">>, [kz_json:set_values(UpdValues, Note)], kz_json:new()),
                    set_note(Cookie, DataBag, Hook)
            end;
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
            DecodedBody = kz_json:decode(Body),
            [Account|_] = kz_json:get_value([<<"response">>, <<"accounts">>], DecodedBody),
            AccountId = kz_json:get_value(<<"id">>, Account),
            UserId = kz_json:get_value([<<"response">>, <<"user">>, <<"id">>], DecodedBody),
            Cookie = props:get_value("set-cookie", Headers),
            {'ok', Cookie, AccountId, UserId};
        {_, Code, _Headers, Body} ->
            lager:info("IAM amocrm_auth Failed Code: ~p Body: ~p",[Code, kz_json:decode(Body)]),
            {'error', 'auth_failed'}
    end.

caller_number(JObj) ->
    kz_json:get_first_defined([<<"caller_id_number">>], JObj).

callee_number(JObj) ->
    [Number|_] = binary:split(kz_json:get_first_defined([<<"to_uri">>,<<"to">>,<<"request">>], JObj), <<"@">>),
    Number.

contact_lookup(Cookie, JObj, #webhook{uri = URL}) ->
    CallerNumber = caller_number(JObj),
    lager:info("IAM contact_lookup CallerNumber: ~p",[CallerNumber]),
    case kz_http:get(<<URL/binary, "/private/api/v2/json/contacts/list?query=", CallerNumber/binary>>
                    ,[{<<"Cookie">>, Cookie}]
                    )
    of
        {'ok', 200, _Headers, Body} ->
            [Contact|_] = kz_json:get_value([<<"response">>,<<"contacts">>], kz_json:decode(Body)),
            lager:info("IAM contact_lookup successful Contact: ~p",[Contact]),
            {'ok', Contact};
        {'ok', 204, _Headers, _Body} ->
            lager:info("IAM contact_lookup returned empty result."),
            {'ok', 'contact_not_found'};
        {_, Code, _Headers, Body} ->
            lager:info("IAM contact_lookup Failed Code: ~p Body: ~p",[Code, kz_json:decode(Body)]),
            {'error', 'contact_lookup_failed'}
    end.

list_notes(Cookie, #webhook{uri = URL}) ->
    IfMS = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(kz_time:current_tstamp()-10800)),
    case kz_http:get(<<URL/binary, "/api/v2/notes?type=contact">>
                     ,[{<<"Cookie">>, Cookie}
                      ,{<<"If-Modified-Since">>, IfMS}
                      ]
                     )
    of
        {'ok', 200, _Headers, Body} ->
            kz_json:get_value([<<"_embedded">>,<<"items">>], kz_json:decode(Body), []);
        {_, Code, _Headers, Body} ->
            lager:info("IAM list_notes Failed Code: ~p Body: ~p",[Code, kz_json:decode(Body)]),
            []
    end.

set_note(Cookie, DataBag, #webhook{uri = URL}) ->
    case kz_http:post(<<URL/binary, "/api/v2/notes">>
                     ,[{<<"Cookie">>, Cookie}]
                     ,DataBag
                     )
    of
        {'ok', 200, _Headers, _Body} ->
            ok;
        {_, Code, _Headers, Body} ->
            lager:info("IAM set_note Failed Code: ~p Body: ~p",[Code, kz_json:decode(Body)]),
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
        ,{[<<"params">>,<<"LINK">>], recording_link(JObj)}
        ,{[<<"params">>,<<"SRC">>], <<"kzonnet">>}
        ]),
    kz_json:set_values(Values, kz_json:new()).

note_lookup(_CallId, []) ->
    'undefined';
note_lookup(CallId, [Note|T]) ->
    case kz_json:get_value([<<"params">>,<<"UNIQ">>], Note) of
        CallId -> kz_json:get_value(<<"id">>, Note);
        _ -> note_lookup(CallId, T)
    end.

call_add(_AmoAccountId, JObj) ->
    AccountId = kz_json:get_value([<<"custom_channel_vars">>, <<"account_id">>], JObj),
    ResellerId = kz_services:find_reseller_id(AccountId),
    Code = kapps_account_config:get(ResellerId, <<"amocrm">>, <<"code">>),
    API_Key = kapps_account_config:get(ResellerId, <<"amocrm">>, <<"api_key">>),
    Url = <<"https://2megarts.amocrm.ru/api/v2/incoming_leads/sip"
           ,"?login=", Code/binary
           ,"&api_key=", API_Key/binary>>,
    DataBag = call_urlencoded_form(JObj),
    case kz_http:post(Url
                     ,[{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}]
                     ,DataBag
                     )
    of
        {'ok', 200, _Headers, _Body} ->
            ok;
        {_, Code, _Headers, Body} ->
            lager:info("IAM call_add Failed. Code: ~p Body: ~p",[Code, kz_json:decode(Body)]),
            {'error', 'call_add_failed'}
    end.

call_urlencoded_form(JObj) ->
    Values =
        [kz_http_util:urlencode(<<"add[0][source_name]">>)
        ,<<"=">>
        ,<<"incoming_call">>
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][source_uid]">>)
        ,<<"=">>
        ,kz_json:get_value(<<"call_id">>, JObj)
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_entities][leads][0][name]">>)
        ,<<"=">>
        ,<<"maybe_a_deal">>
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][to]">>)
        ,<<"=">>
        ,callee_number(JObj)
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][from]">>)
        ,<<"=">>
        ,caller_number(JObj)
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][date_call]">>)
        ,<<"=">>
        ,kz_time:gregorian_seconds_to_unix_seconds(kz_json:get_value(<<"timestamp">>, JObj))
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][duration]">>)
        ,<<"=">>
        ,kz_json:get_value(<<"billing_seconds">>, JObj)
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][link]">>)
        ,<<"=">>
        ,kz_http_util:urlencode(recording_link(JObj))
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][service_code]">>)
        ,<<"=">>
        ,<<"kzonnet">>
        ,<<"&">>
        ,kz_http_util:urlencode(<<"add[0][incoming_lead_info][uniq]">>)
        ,<<"=">>
        ,kz_json:get_value(<<"call_id">>, JObj)
        ],
    kz_binary:join(Values, <<>>).

recording_link(JObj) ->
    AccountId = kz_json:get_value([<<"custom_channel_vars">>, <<"account_id">>], JObj),
    case kz_json:get_value([<<"custom_channel_vars">>,<<"media_recording_id">>], JObj) of
        'undefined' -> 'undefined';
        MediaId ->
            <<"https://beta.onnet.su/kzattachment"
             ,"?account_id=", AccountId/binary
             ,"&doc_type=call_recording"
             ,"&recording_id=", MediaId/binary
             ,"&auth_token=recording">>
    end.

-spec format(kz_json:object()) -> kz_json:object().
format(JObj) ->
    RemoveKeys = [<<"Node">>
                 ,<<"Msg-ID">>
                 ,<<"App-Version">>
                 ,<<"App-Name">>
                 ,<<"Event-Category">>
                 ],
    kz_json:normalize_jobj(JObj, RemoveKeys, []).

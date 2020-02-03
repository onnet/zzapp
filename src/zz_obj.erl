-module(zz_obj).

-export([create_account/3
        ,create_user/2
        ,create_default_callflow/1
        ,collect_onbill_data/1
        ]).

-include_lib("onbill.hrl").

-define(MK_USER,
    {[{<<"call_forward">>,
       {[{<<"substitute">>,false}
        ,{<<"enabled">>,false}
        ,{<<"require_keypress">>,false}
        ,{<<"keep_caller_id">>,false}
        ,{<<"direct_calls_only">>,false}]}}
     ,{<<"enabled">>, 'true'}
     ,{<<"priv_level">>,<<"user">>}
     ,{<<"vm_to_email_enabled">>,true}
     ,{<<"fax_to_email_enabled">>,true}
     ,{<<"verified">>,false}
     ,{<<"timezone">>,<<"UTC">>}
     ,{<<"record_call">>,false}
     ,{<<"pvt_type">>, kzd_user:type()}
     ]}).

-define(DEFAULT_CALLFLOW,
    {[{<<"flow">>,
       {[{<<"children">>,{[]}},
         {<<"data">>,{[]}},
         {<<"module">>,<<"offnet">>}]}},
      {<<"numbers">>,[<<"no_match">>]},
      {<<"patterns">>,[]},
      {<<"pvt_type">>,<<"callflow">>}
    ]}).

-spec create_account(cb_context:context(), kz_json:object(), kz_term:ne_binary()) -> cb_context:context().
create_account(Ctx, JObj, ResellerId) ->
    Tree = crossbar_util:get_tree(ResellerId) ++ [ResellerId],
    Props = [{<<"pvt_type">>, kzd_accounts:type()}
            ,{<<"pvt_tree">>, Tree}
            ],
    Ctx1 = cb_context:set_account_id(Ctx, ResellerId),
    Ctx2 = cb_context:set_doc(Ctx1, kz_json:set_values(Props, JObj)),
    Ctx3 = cb_accounts:put(Ctx2),
    case cb_context:resp_status(Ctx3) of
        'success' ->
            CreatedAccountId = kz_doc:id(cb_context:resp_data(Ctx3)),
            kz_util:spawn(fun create_user/2, [JObj, CreatedAccountId]),
            kz_util:spawn(fun create_default_callflow/1, [Ctx3]),
            kz_util:spawn(fun collect_onbill_data/1, [Ctx3]),
            Ctx3;
        E ->
            lager:info("Create account failed with reason: ~p",[E]),
            Ctx3
    end.

-spec create_user(kz_json:object(), kz_term:ne_binary()) -> cb_context:context().
create_user(JObj, AccountId) ->
    timer:sleep(7000),
    lager:debug("we are about to create user for: ~p",[AccountId]),
    Email = kz_term:to_lower_binary(
       kz_json:get_first_defined([[<<"contact">>,<<"signup">>,<<"email">>]
                                  ,<<"email">>
                                  ]
                                  ,JObj
                                 )
    ),
    Firstname =
       kz_json:get_first_defined([[<<"contact">>,<<"signup">>,<<"first_name">>]
                                  ,<<"first_name">>
                                  ]
                                  ,JObj
    ),
    Surname =
       kz_json:get_first_defined([[<<"contact">>,<<"signup">>,<<"last_name">>]
                                  ,<<"last_name">>
                                  ]
                                  ,JObj
    ),
    Phonenumber =
       kz_json:get_first_defined([[<<"contact">>,<<"signup">>,<<"number">>],<<"number">>], JObj),
    UserPassword = kz_binary:rand_hex(10),
    Props = props:filter_empty([
        {[<<"username">>], Email}
        ,{[<<"first_name">>], Firstname}
        ,{[<<"last_name">>], Surname}
        ,{[<<"email">>], Email}
        ,{[<<"contact_phonenumber">>], Phonenumber}
        ,{[<<"password">>], UserPassword}
        ,{[<<"priv_level">>], <<"admin">>}
        ]),
    Ctx0 = cb_context:set_account_id(cb_context:new(), AccountId),
    Ctx1 = cb_context:set_account_db(Ctx0, kz_util:format_account_db(AccountId)),
    Ctx2 = cb_context:set_doc(Ctx1, kz_json:set_values(Props, ?MK_USER)),
    Ctx3 = cb_context:set_req_data(Ctx2, kz_json:set_values(Props, ?MK_USER)),
    cb_users_v2:create_user(cb_context:set_resp_status(cb_context:set_accepting_charges(Ctx3),'success')).

-spec create_default_callflow(cb_context:context()) -> cb_context:context().
create_default_callflow(Context) ->
    timer:sleep(9000),
    lager:debug("we are about to create default callflow"),
    crossbar_doc:save(cb_context:set_doc(Context, ?DEFAULT_CALLFLOW)).

-spec collect_onbill_data(cb_context:context()) -> cb_context:context().
collect_onbill_data(Context) ->
    timer:sleep(5000),
    ReqData = cb_context:req_data(Context),
    lager:debug("We are about to create onbill doc. ReqData: ~p",[ReqData]),
    AccountId = kz_doc:id(cb_context:doc(Context)),
    ResellerId = zz_util:find_reseller_id(AccountId),
    zz_util:check_db(?ONBILL_DB(ResellerId)),
    Props = props:filter_empty([{<<"account_name">>, kz_json:get_value(<<"companyname">>, ReqData)}
                               ]),
    lager:info("We are about to create onbill doc. Props: ~p",[Props]),
    OnbillDoc = kz_json:from_list(Props),
    lager:debug("We are about to create onbill doc. OnbillDoc: ~p",[OnbillDoc]),
    crossbar_doc:save(cb_context:set_doc(Context
                                        ,kz_doc:set_id(OnbillDoc, <<"onbill">>)
                                        )
                     ),
    crossbar_doc:save(cb_context:set_doc(cb_context:set_account_db(Context, ?ONBILL_DB(ResellerId))
                                        ,kz_doc:set_id(OnbillDoc, AccountId)
                                        )
                     ).


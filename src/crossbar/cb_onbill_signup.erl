-module(cb_onbill_signup).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/2
        ,put/2
        ]).

-export([create_user/1
        ,create_default_callflow/1
        ,collect_onbill_data/1
        ]).

-define(CATEGORY, <<"onbill_signup">>).
-define(MK_DATABAG(JObj), {[{<<"data">>, JObj}]}).

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

-include_lib("crossbar/src/crossbar.hrl").
-include_lib("onbill/src/onbill.hrl").

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_signup">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_signup">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_signup">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.onbill_signup">>, ?MODULE, 'put'),
    ok.

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_AccountId) -> [?HTTP_PUT].

-spec resource_exists(path_tokens()) -> boolean().
resource_exists(?MATCH_ACCOUNT_RAW(_AccountId)) -> 'true';
resource_exists(_) -> 'false'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"onbill_signup">>, [_]}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"onbill_signup">>, [_]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ResellerId) ->
    case kz_services:is_reseller(ResellerId)
         orelse
         cb_context:is_superduper_admin(ResellerId)
    of
        'true' ->
            ReqJSON = cb_context:req_json(Context),
            case maybe_valid_reseller_signup_key(ResellerId, ReqJSON)
            of
                'true' ->
                    cb_accounts:validate(Context);
                'false' ->
                    crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Invalid signup key">>}]}, Context)
            end;
        'false' ->
            crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Reseller status needed">>}]}, Context)
    end.

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ResellerId) ->
    Tree = crossbar_util:get_tree(ResellerId) ++ [ResellerId],
    Props = [{<<"pvt_type">>, kz_account:type()}
            ,{<<"pvt_tree">>, Tree}
            ],
    Ctx1 = cb_context:set_account_id(Context, ResellerId),
    Ctx2 = cb_context:set_doc(Ctx1, kz_json:set_values(Props, cb_context:req_data(Ctx1))),
    lager:debug("we are about to create account"),
    Ctx3 = cb_accounts:put(Ctx2),
    case cb_context:resp_status(Ctx3) of
        'success' ->
            lager:info("account created"),
            kz_util:spawn(fun create_user/1, [Ctx3]),
            kz_util:spawn(fun create_default_callflow/1, [Ctx3]),
            kz_util:spawn(fun collect_onbill_data/1, [Ctx3]),
            Ctx3;
        E ->
            lager:info("Create account failed with reason: ~p",[E]),
            Ctx3
    end.

maybe_valid_reseller_signup_key(ResellerId, ReqJSON) ->
    SignupKey = kz_json:get_value(<<"signup_key">>, ReqJSON),
    ResellerSignupKey = kapps_account_config:get(ResellerId, ?CATEGORY, <<"signup_key">>),
    lager:info("Signup Key: ~p, Reseller Signup Key: ~p",[SignupKey, ResellerSignupKey]),
    SignupKey == ResellerSignupKey.

-spec create_user(cb_context:context()) -> cb_context:context().
create_user(Context) ->
    timer:sleep(7000),
    ReqData = cb_context:req_data(Context),
    AccountId = kz_json:get_value(<<"id">>, cb_context:resp_data(Context)),
    lager:debug("we are about to create user for: ~p",[AccountId]),
    Email = kz_term:to_lower_binary(kz_json:get_value([<<"contact">>,<<"signup">>,<<"email">>], ReqData)),
    Firstname = kz_json:get_value([<<"contact">>,<<"signup">>,<<"first_name">>], ReqData),
    Surname = kz_json:get_value([<<"contact">>,<<"signup">>,<<"last_name">>], ReqData),
    Phonenumber = kz_json:get_value([<<"contact">>,<<"signup">>,<<"number">>], ReqData),
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
    Ctx1 = cb_context:set_account_id(Context, AccountId),
    Ctx2 = cb_context:set_doc(Ctx1, kz_json:set_values(Props, ?MK_USER)),
    Ctx3 = cb_context:set_req_data(Ctx2, kz_json:set_values(Props, ?MK_USER)),
    cb_users_v2:create_user(cb_context:set_accepting_charges(Ctx3)).

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
    ResellerId = kz_services:find_reseller_id(AccountId),
    onbill_util:check_db(?ONBILL_DB(ResellerId)),
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

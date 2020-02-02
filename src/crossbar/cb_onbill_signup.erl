-module(cb_onbill_signup).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/2
        ,put/2
        ]).

-define(CATEGORY, <<"onbill_signup">>).
-define(MK_DATABAG(JObj), {[{<<"data">>, JObj}]}).

-include_lib("crossbar/src/crossbar.hrl").
-include_lib("onbill.hrl").
%-include_lib("onbill/src/onbill.hrl").

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
    case kz_services_reseller:is_reseller(ResellerId)
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
    zz_obj:create_account(Context, cb_context:req_data(Context), ResellerId).

maybe_valid_reseller_signup_key(ResellerId, ReqJSON) ->
    SignupKey = kz_json:get_value(<<"signup_key">>, ReqJSON),
    ResellerSignupKey = kapps_account_config:get(ResellerId, ?CATEGORY, <<"signup_key">>),
    lager:info("Signup Key: ~p, Reseller Signup Key: ~p",[SignupKey, ResellerSignupKey]),
    SignupKey == ResellerSignupKey.

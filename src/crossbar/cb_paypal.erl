-module(cb_paypal).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/2
        ,post/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").
-include("onbill.hrl").


-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.paypal">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.paypal">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.paypal">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.paypal">>, ?MODULE, 'post'),
    ok.

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_AccountId) -> [?HTTP_POST].

-spec resource_exists(path_tokens()) -> boolean().
resource_exists(_AccountId) -> 'true'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"paypal">>, [_]}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"paypal">>, [_]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, AccountId) ->
    lager:info("CB_PayPal validate AccountId: ~p",[AccountId]),
    cb_context:set_resp_status(Context, 'success').

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, AccountId) ->
    lager:info("CB_PayPal post AccountId: ~p",[AccountId]),
    ReqJSON = cb_context:req_json(Context),
    BeneficiaryId = kz_json:get_value(<<"custom">>, ReqJSON),
    Amount = kz_json:get_value(<<"mc_gross">>, ReqJSON),
    PayPalAPI = "https://www.sandbox.paypal.com/cgi-bin/webscr",
    IPNVerifyString = [<<"?cmd=_notify-validate&">>] ++ kz_http_util:json_to_querystring(ReqJSON),
    VerifyReqString = PayPalAPI ++ binary_to_list(iolist_to_binary(IPNVerifyString)),
    lager:info("CB_PayPal post req_json: ~p",[ReqJSON]),
    lager:info("CB_PayPal post BeneficiaryId: ~p",[BeneficiaryId]),
    lager:info("CB_PayPal post Amount: ~p",[Amount]),
    lager:info("CB_PayPal post VerifyReqString: ~p",[VerifyReqString]),
    case kz_http:put(VerifyReqString) of
        {ok,200, _, <<"VERIFIED">>} ->
            lager:info("CB_PayPal post VERIFIED"),
            cb_context:set_resp_status(Context, 'success');
        _ ->
            lager:info("CB_PayPal post INVALID"),
            cb_context:add_system_error('forbidden', Context)
    end.

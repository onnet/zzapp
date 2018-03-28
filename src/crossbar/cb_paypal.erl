%%
%%  Instant Payment Notification (IPN)
%%
%%  https://crossbar_server_with_443_port/hooks/paypal/5c36900d63069782e4e0b0ad2f131e45
%%  

-module(cb_paypal).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/2
        ,post/2
        ]).

-export([add_transaction/1]).

-define(CATEGORY, <<"paypal">>).
-define(PROCESSABLE_PAYMENT_STATUSES, [<<"Completed">>]).
-define(TRANSACTION_ID(TxnId, Year, Month), <<(?TO_BIN(Year))/binary, (kz_date:pad_month(Month))/binary, "-", TxnId/binary>>).

-include_lib("crossbar/src/crossbar.hrl").
-include_lib("onbill/src/onbill.hrl").

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
resource_exists(?MATCH_ACCOUNT_RAW(_AccountId)) -> 'true';
resource_exists(_) -> 'false'.

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
validate(Context, BeneficiaryId) ->
    lager:info("CB_PayPal validate BeneficiaryId: ~p",[BeneficiaryId]),
    case kz_services:is_reseller(BeneficiaryId) of
        'true' ->
            ReqJSON = cb_context:req_json(Context),
            lager:info("CB_PayPal post req_json: ~p",[ReqJSON]),
            case maybe_payment_status_processable(BeneficiaryId, ReqJSON)
                 andalso maybe_valid_receiver_id(BeneficiaryId, ReqJSON)
                 andalso maybe_beneficiary_child(BeneficiaryId, ReqJSON)
                 andalso maybe_valid_currency(BeneficiaryId, ReqJSON)
            of
                'true' ->
                    cb_context:set_resp_status(Context, 'success');
                'false' ->
                    crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Invalid data">>}]}, Context)
            end;
        'false' ->
            crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Non reseller beneficiary">>}]}, Context)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, BeneficiaryId) ->
    ReqJSON = cb_context:req_json(Context),
    lager:info("CB_PayPal post req_json: ~p",[ReqJSON]),
    case maybe_genuine_payment(BeneficiaryId, ReqJSON) of
        'true' ->
            case maybe_transaction_exists(ReqJSON) of
                'true' ->
                    cb_context:set_resp_status(Context, 'success');
                'false' ->
                    kz_util:spawn(fun cb_paypal:add_transaction/1, [ReqJSON]),
                    cb_context:add_system_error('repeat', Context)
            end;
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

maybe_payment_status_processable(_BeneficiaryId, ReqJSON) ->
    PaymentStatus = kz_json:get_value(<<"payment_status">>, ReqJSON),
    lager:info("payment status: ~p",[PaymentStatus]),
    lists:member(PaymentStatus, ?PROCESSABLE_PAYMENT_STATUSES).

maybe_valid_receiver_id(BeneficiaryId, ReqJSON) ->
    PaymentReceiverId = kz_json:get_value(<<"receiver_id">>, ReqJSON),
    BeneficiaryReceiverId = kapps_account_config:get(BeneficiaryId, ?CATEGORY, <<"receiver_id">>),
    lager:info("beneficiary receiver_id: ~p, payer receiver_id: ~p",[BeneficiaryReceiverId, PaymentReceiverId]),
    PaymentReceiverId == BeneficiaryReceiverId.

maybe_beneficiary_child(BeneficiaryId, ReqJSON) ->
    PayerId = kz_json:get_value(<<"custom">>, ReqJSON),
    lager:info("beneficiary_id: ~p, payer_id: ~p",[BeneficiaryId, PayerId]),
    BeneficiaryId == kz_services:find_reseller_id(PayerId).

maybe_valid_currency(BeneficiaryId, ReqJSON) ->
    PaymentCurrency = kz_json:get_value(<<"mc_currency">>, ReqJSON),
    BeneficiaryCurrency = kapps_account_config:get(BeneficiaryId, ?CATEGORY, <<"currency">>),
    lager:info("beneficiary currency: ~p, payer currency: ~p",[BeneficiaryCurrency, PaymentCurrency]),
    PaymentCurrency == BeneficiaryCurrency.

maybe_genuine_payment(BeneficiaryId, ReqJSON) ->
    PayPalAPI =
        case kapps_account_config:get(BeneficiaryId, ?CATEGORY, <<"environment">>) of
            <<"sandbox">> -> "https://www.sandbox.paypal.com/cgi-bin/webscr";
            <<"production">> -> "https://www.paypal.com/cgi-bin/webscr"
        end,
    IPNVerifyString = [<<"?cmd=_notify-validate&">>] ++ kz_http_util:json_to_querystring(ReqJSON),
    VerifyReqString = PayPalAPI ++ binary_to_list(iolist_to_binary(IPNVerifyString)),
    case kz_http:put(VerifyReqString) of
        {ok,200, _, <<"VERIFIED">>} ->
            'true';
        _ ->
            lager:info("payment request is fake, check made against ~p",[PayPalAPI]),
            'false'
    end.

maybe_transaction_exists(ReqJSON) ->
    {Year, Month, _} = erlang:date(),
    PayerId = kz_json:get_value(<<"custom">>, ReqJSON),
    TxnId = kz_json:get_value(<<"txn_id">>, ReqJSON),
    maybe_transaction_exists(PayerId, TxnId, Year, Month).

maybe_transaction_exists(PayerId, TxnId, Year, Month) ->
    case kz_datamgr:open_doc(kazoo_modb:get_modb(PayerId, Year, Month)
                            ,?TRANSACTION_ID(TxnId, Year, Month))
    of
        {ok, _} ->
            'true';
        {'error', 'not_found'} ->
            {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            case kz_datamgr:open_doc(kazoo_modb:get_modb(PayerId, PrevYear, PrevMonth)
                                    ,?TRANSACTION_ID(TxnId, PrevYear, PrevMonth))
            of
                {ok, _} ->
                    'true';
                _ ->
                    'false'
            end
    end.

-spec add_transaction(kz_json:object()) -> any().
add_transaction(ReqJSON) ->
    {Year, Month, _} = erlang:date(),
    PayerId = kz_json:get_value(<<"custom">>, ReqJSON),
    TxnId = kz_json:get_value(<<"txn_id">>, ReqJSON),
    Modb = kazoo_modb:get_modb(PayerId, Year, Month),
    Timestamp = kz_time:current_tstamp(),
    Values = [{<<"_id">>, ?TRANSACTION_ID(TxnId, Year, Month)}
             ,{<<"ipn_databag">>, ReqJSON}
             ,{<<"pvt_amount">>, wht_util:dollars_to_units(kz_json:get_value(<<"mc_gross">>, ReqJSON))}
             ,{<<"pvt_type">>, <<"credit">>}
             ,{<<"pvt_created">>, Timestamp}
             ,{<<"pvt_modified">>, Timestamp}
             ,{<<"description">>, <<"PayPal transaction">>}
             ,{<<"pvt_reason">>, <<"paypal_ipn_transaction">>}],
    kz_datamgr:ensure_saved(Modb, kz_json:set_values(Values, kz_json:new())).


%%
%%  Yandex.Money check/paymentAviso module
%%
%%  https://crossbar_server:8443/hooks/yandex_money/check
%%  https://crossbar_server:8443/hooks/yandex_money/paymentaviso
%%  
%%  GET method added just as a cheatsheet

-module(cb_yandex_money).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/2
        ,post/2

        ,to_xml/1
        ]).

-export([add_transaction/1]).
-export([maybe_transaction_exists/1]).

%%-define(XML_CONTENT_TYPES, [{<<"application">>, <<"xml">>}
%%                           ,{<<"text">>, <<"xml">>}
%%                           ]).

-define(XML_HEAD, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>).
-define(CATEGORY, <<"yandex_money">>).
-define(CHECK, <<"check">>).
-define(PAYMENT_AVISO, <<"paymentaviso">>).

-define(PROCESSABLE_PAYMENT_STATUSES, [<<"Completed">>]).
-define(TRANSACTION_ID(TxnId), <<"yandex-money-", TxnId/binary>>).

-include_lib("crossbar/src/crossbar.hrl").
-include_lib("onbill.hrl").
% -include_lib("onbill/src/onbill.hrl").

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.yandex_money">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.yandex_money">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.yandex_money">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.yandex_money">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.yandex_money">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.to_xml.get.yandex_money">>, ?MODULE, 'to_xml'),
    ok.

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) -> [?HTTP_GET, ?HTTP_POST].

-spec resource_exists(path_tokens()) -> boolean().
resource_exists(?CHECK) -> 'true';
resource_exists(?PAYMENT_AVISO) -> 'true';
resource_exists(_) -> 'false'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"yandex_money">>, [_]}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"yandex_money">>, [_]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context) ->
    ctp(Context).
content_types_provided(Context, _TaskId) ->
    ctp(Context).
content_types_provided(Context, _TaskId, _CSV) ->
    ctp(Context).

-spec ctp(cb_context:context()) -> cb_context:context().
ctp(Context) ->
    cb_context:add_content_types_provided(Context, [{'to_xml', ?XML_CONTENT_TYPES}]).

%%
%% to_xml just an example of a GET requests service and doesn't used by Yandex.Money
%% 
-spec to_xml({cowboy_req:req(), cb_context:context()}) ->
                    {cowboy_req:req(), cb_context:context()}.
to_xml({Req, Context}) ->
    Data =
       <<?XML_HEAD/binary
        ,"<response performedDatetime='"
        ,(kz_term:to_binary(get_local_datetime()))/binary
        ,"' message='GET action forbidden' />"
       >>,
    {Req, cb_context:set_resp_data(Context, Data)}.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CHECK) ->
    ReqJSON = cb_context:req_json(Context),
    lager:info("CB_YandexMoney CHECK validate req_json: ~p",[ReqJSON]),
    cb_context:set_resp_status(Context, 'success');
validate(Context, ?PAYMENT_AVISO) ->
    ReqJSON = cb_context:req_json(Context),
    lager:info("CB_YandexMoney PAYMENT_AVISO post req_json: ~p",[ReqJSON]),
    cb_context:set_resp_status(Context, 'success').

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _BeneficiaryId) ->
    ReqJSON = cb_context:req_json(Context),
    lager:info("CB_PayPal post req_json: ~p",[ReqJSON]),
    Data =
        case kz_json:get_binary_value(<<"action">>, ReqJSON, <<>>) of
            <<"checkOrder">> -> validate_check(Context);
            <<"paymentAviso">> -> validate_payment_aviso(Context);
            _ ->
                <<?XML_HEAD/binary
                 ,"<response performedDatetime='"
                 ,(kz_term:to_binary(get_local_datetime()))/binary
                 ,"' message='No action found' />"
                >>
        end,
    lager:info("IAMXML post response  Data: ~p",[Data]),
    cb_context:set_resp_data(cb_context:set_resp_status(Context, 'success'), Data).

validate_check(Context) ->
    ReqJSON = cb_context:req_json(Context),
    ShopId = kz_json:get_binary_value(<<"shopId">>, ReqJSON, <<>>),
    InvoiceId = kz_json:get_binary_value(<<"invoiceId">>, ReqJSON, <<>>),
    PerformedDateTime = kz_term:to_binary(get_local_datetime()),
    case is_key_valid(Context)
 %        andalso
 %        lb_util:is_valid_agrm_id(CustomerNumber, Context)
    of
        'true' ->
            <<?XML_HEAD/binary
             ,"<checkOrderResponse performedDatetime='"
             ,PerformedDateTime/binary
             ,"' code='0' invoiceId='"
             ,InvoiceId/binary
             ,"' shopId='"
             ,ShopId/binary
             ,"' />"
            >>;
        'false' ->
            <<?XML_HEAD/binary
             ,"<checkOrderResponse performedDatetime='"
             ,PerformedDateTime/binary
             ,"' code='1' invoiceId='"
             ,InvoiceId/binary
             ,"' shopId='"
             ,ShopId/binary
             ,"' message='Wrong request' />"
            >>
    end.

validate_payment_aviso(Context) ->
    ReqJSON = cb_context:req_json(Context),
    ShopId = kz_json:get_binary_value(<<"shopId">>, ReqJSON, <<>>),
    InvoiceId = kz_json:get_binary_value(<<"invoiceId">>, ReqJSON, <<>>),
    PerformedDateTime = kz_term:to_binary(get_local_datetime()),
    case is_key_valid(Context)
 %        andalso
 %        lb_util:is_valid_agrm_id(CustomerNumber, Context)
    of
        'true' ->
            case maybe_transaction_exists(ReqJSON) of
                'true' ->
                    <<?XML_HEAD/binary
                     ,"<checkOrderResponse performedDatetime='"
                     ,PerformedDateTime/binary
                     ,"' code='0' invoiceId='"
                     ,InvoiceId/binary
                     ,"' shopId='"
                     ,ShopId/binary
                     ,"' />"
                    >>;
                'false' ->
                    kz_util:spawn(fun cb_yandex_money:add_transaction/1, [ReqJSON]),
                    <<?XML_HEAD/binary
                     ,"<checkOrderResponse performedDatetime='"
                     ,PerformedDateTime/binary
                     ,"' code='repeat ' invoiceId='"
                     ,InvoiceId/binary
                     ,"' shopId='"
                     ,ShopId/binary
                     ,"' />"
                    >>
            end;
        'false' ->
            <<?XML_HEAD/binary
             ,"<checkOrderResponse performedDatetime='"
             ,PerformedDateTime/binary
             ,"' code='1' invoiceId='"
             ,InvoiceId/binary
             ,"' shopId='"
             ,ShopId/binary
             ,"' message='Wrong request' />"
            >>
    end.

get_request_vars(ReqJSON) ->
    [kz_json:get_binary_value(<<"action">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"orderSumAmount">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"orderSumCurrencyPaycash">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"orderSumBankPaycash">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"shopId">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"invoiceId">>, ReqJSON, <<>>)
    ,kz_json:get_binary_value(<<"customerNumber">>, ReqJSON, <<>>)
    ].

is_key_valid(Context) ->
    ReqJSON = cb_context:req_json(Context),
    case kz_json:get_binary_value(<<"kz_account_id">>, ReqJSON) of
        'undefined' -> 'false';
        AccountId ->
            ResellerId = onbill_util:find_reseller_id(AccountId),
            ShopPassword = kapps_account_config:get(ResellerId, ?CATEGORY, <<"shop_pwd">>),
            ControlString = io_lib:format("~s;~s;~s;~s;~s;~s;~s;~s", (get_request_vars(ReqJSON) ++ [ShopPassword])),
            Md5Hash = lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(ControlString)]),
            case kz_json:get_binary_value(<<"md5">>, ReqJSON, <<>>) of
                undefined -> 'false';
                Md5 -> string:to_lower(Md5Hash) == string:to_lower(kz_term:to_list(Md5))
            end
    end.

-spec maybe_transaction_exists(any()) -> any().
maybe_transaction_exists(ReqJSON) ->
    {Year, Month, _} = erlang:date(),
    PayerId = kz_json:get_value(<<"kz_account_id">>, ReqJSON),
    TxnId = kz_json:get_value(<<"yandexPaymentId">>, ReqJSON),
    maybe_transaction_exists(PayerId, TxnId, Year, Month).

maybe_transaction_exists(PayerId, TxnId, Year, Month) ->
    case kz_datamgr:open_doc(kazoo_modb:get_modb(PayerId, Year, Month)
                            ,?TRANSACTION_ID(TxnId))
    of
        {ok, _} ->
            'true';
        {'error', 'not_found'} ->
            {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            case kz_datamgr:open_doc(kazoo_modb:get_modb(PayerId, PrevYear, PrevMonth)
                                    ,?TRANSACTION_ID(TxnId))
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
    PayerId = kz_json:get_value(<<"kz_account_id">>, ReqJSON),
    TxnId = kz_json:get_value(<<"yandexPaymentId">>, ReqJSON),
    Modb = kazoo_modb:get_modb(PayerId, Year, Month),
    Timestamp = kz_time:current_tstamp(),
    Values = [{<<"_id">>, ?TRANSACTION_ID(TxnId)}
             ,{<<"aviso_databag">>, ReqJSON}
             ,{<<"pvt_amount">>, kz_currency:dollars_to_units(kz_json:get_value(<<"orderSumAmount">>, ReqJSON))}
             ,{<<"pvt_type">>, <<"credit">>}
             ,{<<"pvt_created">>, Timestamp}
             ,{<<"pvt_modified">>, Timestamp}
             ,{<<"description">>, <<"Yandex.Money transaction">>}
             ,{<<"pvt_reason">>, <<"yandex_money_transaction">>}],
    kz_datamgr:ensure_saved(Modb, kz_json:set_values(Values, kz_json:new())).

get_local_datetime() ->
    {_, _, Usec} = Now = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
    TZ = lists:sublist(os:cmd("date +%z"), 3) ++ ":" ++ lists:sublist(os:cmd("date +%z"), 4, 2),
    io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w~s", [ Y, Mo, D, H, Mi, S, Usec, TZ]).


-module(cb_onbill_pvt_limits).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,content_types_provided/1
         ,validate/1
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(LIMITS_DOC, <<"limits">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_pvt_limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_pvt_limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_pvt_limits">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_pvt_limits">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_limits(Context, cb_context:req_verb(Context)).

-spec validate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_limits(Context, ?HTTP_GET) ->
    ResellerId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
            leak_limits(AccountId, Context);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end;

validate_limits(Context, ?HTTP_POST) ->
    ResellerId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
            set_pvt_values(AccountId, Context);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

leak_limits(AccountId, Context) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, ?LIMITS_DOC) of
        {'error', _R} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_data/2, kz_json:new()}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        {'ok', JObj} ->
            Values =
                props:filter_undefined(
                    [{<<"allow_postpay">>,kz_json:get_value(<<"pvt_allow_postpay">>, JObj)}
                    ,{<<"max_postpay_amount">>,kz_json:get_value(<<"pvt_max_postpay_amount">>, JObj)}
                    ,{<<"twoway_trunks">>,kz_json:get_value(<<"pvt_twoway_trunks">>, JObj)}
                    ,{<<"outbound_trunks">>,kz_json:get_value(<<"pvt_outbound_trunks">>, JObj)}
                    ,{<<"inbound_trunks">>,kz_json:get_value(<<"pvt_inbound_trunks">>, JObj)}
                    ]),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_data/2, kz_json:from_list(Values)}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ])
    end.

set_pvt_values(AccountId, Context) ->
    ReqData = cb_context:req_data(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Doc = case kz_datamgr:open_doc(AccountDb, ?LIMITS_DOC) of
              {'error', _} ->
                  {[{<<"_id">>, <<"limits">>}]};
              {'ok', JObj} ->
                  JObj
          end,
    Values =
        props:filter_undefined(
            [{<<"pvt_allow_postpay">>,kz_json:get_value(<<"allow_postpay">>, ReqData)}
            ,{<<"pvt_max_postpay_amount">>,kz_json:get_value(<<"max_postpay_amount">>, ReqData)}
            ,{<<"pvt_twoway_trunks">>,kz_json:get_value(<<"twoway_trunks">>, ReqData)}
            ,{<<"pvt_outbound_trunks">>,kz_json:get_value(<<"outbound_trunks">>, ReqData)}
            ,{<<"pvt_inbound_trunks">>,kz_json:get_value(<<"inbound_trunks">>, ReqData)}
            ]),
    case kz_datamgr:ensure_saved(AccountDb, kz_json:set_values(Values, Doc)) of
        {'error', _} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_data/2, kz_json:new()}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        {'ok', _} ->
            leak_limits(AccountId, Context)
    end.

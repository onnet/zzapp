%%%-----------------------------------------------------------
%%%
%%% Just to be able to edit service_plan docs
%%% Didn't found this functionality in stock cb_service_plans
%%%
%%%-----------------------------------------------------------

-module(cb_onbill_service_plans).

-export([init/0
         ,authorize/1
         ,allowed_methods/1
         ,resource_exists/1
         ,content_types_provided/2
         ,validate/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").
-type authorize_return() :: boolean() | {'stop', cb_context:context()}.

-define(CB_LIST, <<"services/plans">>).
-define(SERVICE_PLAN_DOC_TYPE, <<"service_plan">>).
-define(SYNC_RATEDECKS_PLANS, <<"sync_ratedeck_plans">>).
-define(RATE_SP_DOC(RDeckId),
    {[{<<"_id">>,RDeckId},
      {<<"plan">>,{[]}},
      {<<"pvt_type">>,<<"service_plan">>},
      {<<"name">>,<<"Ratedeck ", RDeckId/binary, " Service Plan">>},
      {<<"applications">>,{[]}},
      {<<"ratedeck">>,
       {[{<<"id">>, RDeckId}]}}]}
).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize.onbill_service_plans">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_service_plans">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_service_plans">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_service_plans">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_service_plans">>, ?MODULE, 'validate').

-spec authorize(cb_context:context()) -> authorize_return().
authorize(Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context,_) ->
    Context.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, [{'expected_type', ?SERVICE_PLAN_DOC_TYPE}]);
validate_onbill(Context, ?SYNC_RATEDECKS_PLANS, ?HTTP_POST) ->
    Ratedecks = [remove_ratedeck_prefix(Ratedeck)
                 || Ratedeck <- kz_services_ratedecks:ratedecks() -- [<<"ratedeck">>]
                ], 
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services_reseller:get_id(AccountId),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    C1 = crossbar_doc:load_view(?CB_LIST
                               ,[]
                               ,cb_context:set_account_db(Context, ResellerDb)
                               ,fun normalize_available_view_results/2
                               ),
    PlanIds = [kz_json:get_value(<<"id">>, JObj) || JObj <- cb_context:doc(C1)],
    [create_rd_serviceplan(RD, Context) || RD <- Ratedecks, not lists:member(RD, PlanIds)],
    cb_context:set_resp_status(Context, 'success');

validate_onbill(Context, Id, ?HTTP_POST) ->
    save(Id, kz_util:format_account_id(cb_context:account_id(Context),'encoded'), Context).

-spec save(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
save(Id, DbName, Context) ->
    ReqData = kz_json:delete_key(<<"id">>, cb_context:req_data(Context)),
    Doc = case kz_datamgr:open_doc(DbName, Id) of
              {'ok', JObj} -> JObj;
              {error,not_found} ->
                  InitValues = props:filter_undefined([{<<"_id">>, Id}
                                                  ,{<<"pvt_type">>, ?SERVICE_PLAN_DOC_TYPE}
                                                  ]),
                  kz_json:set_values(InitValues, kz_json:new())
          end,
    Values = kz_json:to_proplist(kz_doc:private_fields(Doc)),
    NewDoc = kz_json:set_values(Values, ReqData),
    Context1 = cb_context:set_doc(Context, NewDoc),
    crossbar_doc:save(cb_context:set_account_db(Context1, DbName)).

-spec normalize_available_view_results(kz_json:object(), kz_json:objects()) ->
                                              kz_json:objects().
normalize_available_view_results(JObj, Acc) ->
    [kz_json:get_json_value(<<"value">>, JObj)|Acc].

remove_ratedeck_prefix(<<"ratedeck%2F", N/binary>>) -> N;
remove_ratedeck_prefix(NoPref) -> NoPref.

create_rd_serviceplan(RDeckId, Context) ->
    ResellerId = kz_services_reseller:get_id(cb_context:account_id(Context)),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    Context1 = cb_context:set_doc(Context, ?RATE_SP_DOC(RDeckId)),
    crossbar_doc:save(cb_context:set_account_db(Context1, ResellerDb)).


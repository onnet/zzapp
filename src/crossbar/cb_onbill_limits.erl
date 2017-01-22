-module(cb_onbill_limits).

-export([init/0
         ,allowed_methods/1
         ,resource_exists/1
         ,content_types_provided/2
         ,validate/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(SERVICE_PLAN_DOC_TYPE, <<"service_plan">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_limits">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_limits">>, ?MODULE, 'validate').

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
validate_onbill(Context, Id, ?HTTP_POST) ->
    save(Id, kz_util:format_account_id(cb_context:account_id(Context),'encoded'), Context).

-spec save(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
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

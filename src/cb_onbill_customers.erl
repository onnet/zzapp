%%%----------------------------------------------------------------------------
%%%
%%% Saves/retrieves account variables used for accounting docs generation
%%% These variables are injected into template on each erlydtl doc generation,
%%% so each var added here could be used in doc template
%%%
%%%----------------------------------------------------------------------------

-module(cb_onbill_customers).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,content_types_provided/1
         ,validate/1
        ]).

-include("../../crossbar/src/crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_customers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_customers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_customers">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_customers">>, ?MODULE, 'validate').

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
    validate_onbill(Context, cb_context:req_verb(Context)).

validate_onbill(Context, ?HTTP_GET) ->
    AccDoc = cb_context:account_doc(Context),
    JObj = case kz_json:get_value(<<"pvt_onbill_account_vars">>, AccDoc) of
                'undefined' -> kz_json:new();
                AccJObj -> AccJObj
           end,
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:public_fields(JObj)}
                       ]);
validate_onbill(Context, ?HTTP_POST) ->
    AccDoc = cb_context:account_doc(Context),
    JObj = kz_json:set_value(<<"pvt_onbill_account_vars">>, cb_context:req_data(Context), AccDoc),
    crossbar_doc:save(cb_context:set_doc(Context, JObj)).

%%
%% We don't need system/reseller configs merge here, therefore can't use cb_configs
%%

-module(cb_paypal_config).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ]).

-define(MERCHANT_INFO, <<"merchant_info">>).
-define(PAYPAL_CONFIG_ID, <<"configs_paypal">>).

-include_lib("crossbar/src/crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.paypal_config">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.paypal_config">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.paypal_config">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(?MERCHANT_INFO) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?MERCHANT_INFO) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_paypal_config(Context, cb_context:req_verb(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?MERCHANT_INFO) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    crossbar_doc:load(?PAYPAL_CONFIG_ID, cb_context:set_account_db(Context, ResellerDb));
validate(Context, _) ->
    Context.

-spec validate_paypal_config(cb_context:context(), path_token()) -> cb_context:context().
validate_paypal_config(Context, ?HTTP_GET) ->
    maybe_handle_load_failure(crossbar_doc:load(?PAYPAL_CONFIG_ID, Context));
validate_paypal_config(Context, ?HTTP_POST) ->
    save_paypal_config(Context).

-spec save_paypal_config(cb_context:context()) -> cb_context:context().
save_paypal_config(Context) ->
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    Db = kz_util:format_account_id(AccountId, 'encoded'),
    Rev = case kz_datamgr:lookup_doc_rev(Db, ?PAYPAL_CONFIG_ID) of
              {'ok', Rv} -> Rv;
              _ -> 'undefined'
          end,
    Values = props:filter_undefined([{<<"_id">>, ?PAYPAL_CONFIG_ID}
                                    ,{<<"_rev">>, Rev}
                                    ]),
    crossbar_doc:save(cb_context:set_doc(Context, kz_json:set_values(Values, ReqData))).

-spec maybe_valid_relationship(cb_context:context()) -> boolean().
maybe_valid_relationship(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    cb_context:is_superduper_admin(AuthAccountId)
      orelse (kz_services_reseller:is_reseller(AuthAccountId)
                andalso AuthAccountId == AccountId).

-spec maybe_handle_load_failure(cb_context:context()) -> cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) -> cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:new()}
                       ,{fun cb_context:set_doc/2, kz_json:new()}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.


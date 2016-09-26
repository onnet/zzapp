-module(cb_periodic_fees).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
        ]).

-include("../../crossbar/src/crossbar.hrl").

-define(CB_PERIODIC_FEES, <<"periodic_fees/crossbar_listing">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.periodic_fees">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.periodic_fees">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.periodic_fees">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    case maybe_valid_relationship(Context) of
        'true' ->
            AccountId = cb_context:account_id(Context),
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            _ = onbill_util:maybe_add_design_doc(AccountDb, <<"periodic_fees">>),
            validate_periodic_fees(Context, cb_context:req_verb(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.
validate(Context, Id) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_periodic_fees(Context, Id, cb_context:req_verb(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

-spec validate_periodic_fees(cb_context:context(), path_token()) -> cb_context:context().
validate_periodic_fees(Context, ?HTTP_GET) ->
    case cb_context:req_value(Context, <<"active_only">>) of
        <<"true">> ->
            crossbar_doc:load_view(?CB_PERIODIC_FEES, [], Context, fun onbill_util:normalize_view_active_results/2);
        _ ->
            crossbar_doc:load_view(?CB_PERIODIC_FEES, [], Context, fun onbill_util:normalize_view_results/2)
    end;
validate_periodic_fees(Context, ?HTTP_PUT) ->
    save_periodic_fees(Context).

-spec validate_periodic_fees(cb_context:context(), ne_binary(), path_token()) -> cb_context:context().
validate_periodic_fees(Context, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, [{'expected_type', <<"periodic_fee">>}]);
validate_periodic_fees(Context, Id, ?HTTP_POST) ->
    save_periodic_fees(Context, Id).

-spec save_periodic_fees(cb_context:context()) -> cb_context:context().
-spec save_periodic_fees(cb_context:context(), ne_binary()) -> cb_context:context().
save_periodic_fees(Context) ->
    save_periodic_fees(Context, kz_datamgr:get_uuid()).
save_periodic_fees(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    ReqData = cb_context:req_data(Context),
    Db = kz_util:format_account_id(AccountId, 'encoded'),
    Rev = case kz_datamgr:lookup_doc_rev(Db, Id) of
              {'ok', Rv} -> Rv;
              _ -> 'undefined'
          end,
    Values = props:filter_undefined([{<<"_id">>, Id}
                                    ,{<<"_rev">>, Rev}
                                    ,{<<"pvt_type">>, <<"periodic_fee">>}
                                    ]),
    NewDoc = kz_json:set_values(Values, ReqData),
    Context2 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    %% Don't want to extend crossbar_services now
    _ = kz_services:reconcile(AccountId),
    kz_services:save_as_dirty(AccountId),
    Context2.

-spec maybe_valid_relationship(cb_context:context()) -> boolean().
maybe_valid_relationship(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    onbill_util:validate_relationship(AccountId, AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId).

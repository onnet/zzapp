-module(cb_onbill_transactions).

-export([init/0
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/2
        ]).

-include("../../crossbar/src/crossbar.hrl").

-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_transactions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_transactions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_transactions">>, ?MODULE, 'validate').

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET].

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_periodic_fees(Context, Id, cb_context:req_verb(Context));
        'false' ->  cb_context:add_system_error('forbidden', Context)
    end.

-spec validate_periodic_fees(cb_context:context(), ne_binary(), path_token()) -> cb_context:context().
validate_periodic_fees(Context, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, [{'expected_type', <<"debit">>}]).

-spec maybe_valid_relationship(cb_context:context()) -> boolean().
maybe_valid_relationship(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    onbill_util:validate_relationship(AccountId, AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId).

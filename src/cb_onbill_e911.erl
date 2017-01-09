%%%-----------------------------------------------------------
%%%
%%% Manage uploaded E911 proof of address information
%%%
%%%-----------------------------------------------------------

-module(cb_onbill_e911).

-export([init/0
         ,allowed_methods/0,allowed_methods/1
         ,resource_exists/0,resource_exists/1
         ,validate/1,validate/2
        ]).

-include("../../crossbar/src/crossbar.hrl").

-define(CB_E911_ADDRESSES, <<"e911/addresses">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_e911">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_e911">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_e911">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_e911(Context, cb_context:req_verb(Context));
        'false' ->  cb_context:add_system_error('forbidden', Context)
    end.
validate(Context, Id) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_e911_doc(Context, Id, cb_context:req_verb(Context));
        'false' ->  cb_context:add_system_error('forbidden', Context)
    end.

-spec validate_e911(cb_context:context(), http_method()) -> cb_context:context().
validate_e911(Context, ?HTTP_GET) ->
    e911_addresses_summary(Context).

-spec onbills_modb_summary(cb_context:context()) -> cb_context:context().
onbills_modb_summary(Context) ->
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    onbill_util:maybe_add_design_doc(Modb, <<"onbills">>),
    Context1 = cb_context:set_account_db(Context, Modb),
    crossbar_doc:load_view(?CB_LIST, [], Context, fun onbill_util:normalize_view_results/2).

-spec validate_periodic_fees(cb_context:context(), ne_binary(), path_token()) -> cb_context:context().
validate_e911_doc(Context, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id,[{'expected_type', <<"e911_address">>}]).

-spec maybe_valid_relationship(cb_context:context()) -> boolean().
maybe_valid_relationship(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    onbill_util:validate_relationship(AccountId, AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId).

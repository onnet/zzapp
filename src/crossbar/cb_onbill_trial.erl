-module(cb_onbill_trial).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,content_types_provided/1
         ,validate/1
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(KEY_TRIAL_EXPIRATION, <<"pvt_trial_expires">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_trial">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_trial">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_trial">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_trial">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_POST, ?HTTP_DELETE].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_trial(Context, cb_context:req_verb(Context)).

-spec validate_trial(cb_context:context(), http_method()) -> cb_context:context().
validate_trial(Context, ?HTTP_DELETE) ->
    ResellerId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
            {'ok', Doc} = kz_account:fetch(AccountId),
            NewDoc = kz_json:delete_key(?KEY_TRIAL_EXPIRATION, Doc),
            Context1 = cb_context:set_doc(Context, NewDoc),
            cb_context:set_resp_status(crossbar_doc:save(Context1), 'success');
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end;
validate_trial(Context, ?HTTP_POST) ->
    ResellerId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    case onbill_util:validate_relationship(AccountId, ResellerId) of
        'true' ->
            case kz_json:get_value(<<"new_expiration_timestamp">>, cb_context:req_data(Context)) of
                'undefined' ->
                    cb_context:add_system_error('forbidden', Context);
                NewTS ->
                    {'ok', Doc} = kz_account:fetch(AccountId),
                    NewDoc = kz_json:set_value(?KEY_TRIAL_EXPIRATION, kz_util:to_integer(NewTS), Doc),
                    Context1 = cb_context:set_doc(Context, NewDoc),
                    cb_context:set_resp_status(crossbar_doc:save(Context1),'success')
            end;
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

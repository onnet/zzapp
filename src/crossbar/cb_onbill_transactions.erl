%%%-----------------------------------------------------------
%%%
%%% Just to be able to retrieve transaction over API
%%% Didn't found this functionality in stock cb_transactions
%%%
%%%-----------------------------------------------------------

-module(cb_onbill_transactions).

-export([init/0
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(TRANSACTION_TYPES, [<<"credit">>, <<"debit">>]).
-define(DELETABLE_REASONS, [<<"wire_transfer">>, <<"balance_correction">>]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_transactions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_transactions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_transactions">>, ?MODULE, 'validate').

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_DELETE].

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    case maybe_valid_relationship(Context) of
        'true' -> validate_transaction(Context, Id, cb_context:req_verb(Context));
        'false' ->  cb_context:add_system_error('forbidden', Context)
    end.

-spec validate_transaction(cb_context:context(), ne_binary(), path_token()) -> cb_context:context().
validate_transaction(Context, <<Year:4/binary, Month:2/binary, _:2/binary, "-dailyfee">> = Id, ?HTTP_GET) ->
    leak_job_fields(crossbar_doc:load(Id
                                     ,cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month))
                                     ,[{'expected_type', <<"debit">>}]
                                     ));
validate_transaction(Context, <<Year:4/binary, Month:2/binary, _/binary>> = Id, ?HTTP_GET) ->
    crossbar_doc:load(Id
                     ,cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month))
                     ,[{'expected_type', ?TRANSACTION_TYPES}]
                     );
validate_transaction(Context, <<Year:4/binary, Month:2/binary, _/binary>> = Id, ?HTTP_DELETE) ->
    Ctx1 = crossbar_doc:load(Id
                            ,cb_context:set_account_modb(Context, kz_term:to_integer(Year), kz_term:to_integer(Month))
                            ,[{'expected_type', ?TRANSACTION_TYPES}]
                            ),
    JObj = cb_context:doc(Ctx1),
    case lists:member(kz_json:get_value(<<"pvt_reason">>, JObj), ?DELETABLE_REASONS) of
        'true' -> crossbar_doc:delete(Ctx1);
        'false' -> cb_context:add_system_error('forbidden', Ctx1)
    end.

-spec maybe_valid_relationship(cb_context:context()) -> boolean().
maybe_valid_relationship(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    onbill_util:validate_relationship(AccountId, AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId).

-spec leak_job_fields(cb_context:context()) -> cb_context:context().
leak_job_fields(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            JObj = cb_context:doc(Context),
            cb_context:set_resp_data(Context
                                    ,kz_json:set_values([{<<"status">>, kz_json:get_value(<<"pvt_metadata">>, JObj)}
                                                        ], cb_context:resp_data(Context))
                                    );
        _Status -> Context
    end.

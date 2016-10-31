-module(cb_onbills).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/1, content_types_provided/3, content_types_provided/4
         ,content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
        ]).

-include("../../crossbar/src/crossbar.hrl").

-define(CB_LIST, <<"onbills/crossbar_listing">>).
-define(ATTACHMENT, <<"attachment">>).
-define(GENERATE, <<"generate">>).
-define(CARRIERS, <<"carriers">>).
-define(CUSTOMERS, <<"customers">>).
-define(SERVICE_PLANS, <<"onbill_service_plans">>).
-define(MODB, <<"onbills_modb">>).
-define(RESELLER_VARIABLES, <<"onbill_reseller_variables">>).
-define(ALL_CHILDREN, <<"all_children">>).
-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).
-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                               %   ,{<<"text">>, <<"plain">>}
                                 ]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbills">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbills">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbills">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.onbills">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.onbills">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(?RESELLER_VARIABLES) ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(?CARRIERS,_) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?CUSTOMERS,_) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?SERVICE_PLANS,_) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?GENERATE,_) ->
    [?HTTP_PUT];
allowed_methods(?MODB,_) ->
    [?HTTP_GET].
allowed_methods(?CARRIERS,_,_) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?MODB,_,?ATTACHMENT) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?CARRIERS,_) -> 'true';
resource_exists(?CUSTOMERS,_) -> 'true';
resource_exists(?SERVICE_PLANS,_) -> 'true';
resource_exists(?GENERATE,_) -> 'true';
resource_exists(?MODB,_) -> 'true'.
resource_exists(?CARRIERS,_,_) -> 'true';
resource_exists(?MODB,_,?ATTACHMENT) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_,?GENERATE) ->
    Context.
content_types_provided(Context,?CARRIERS,_,_) ->
    CTP = [{'to_binary', [{<<"text">>, <<"html">>}]}],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided(Context,?MODB,_,?ATTACHMENT) ->
    CTP = [{'to_binary', [{<<"application">>, <<"pdf">>}]}],
    cb_context:set_content_types_provided(Context, CTP).

-spec content_types_accepted(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context, ?CARRIERS,_,_) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?NOTIFICATION_MIME_TYPES}
           ,{'from_json', ?JSON_CONTENT_TYPES}
          ],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbill(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).
validate(Context, ?GENERATE, Id) ->
    validate_generate(Context, Id, cb_context:req_verb(Context));
validate(Context, ?CARRIERS, Id) ->
    validate_onbill(Context, ?CARRIERS, Id, cb_context:req_verb(Context));
validate(Context, ?CUSTOMERS, Id) ->
    validate_onbill(Context, ?CUSTOMERS, Id, cb_context:req_verb(Context));
validate(Context, ?SERVICE_PLANS, Id) ->
    validate_onbill(Context, ?SERVICE_PLANS, Id, cb_context:req_verb(Context)).
validate(Context,?CARRIERS, Id, AttachmentId) ->
    validate_onbill(Context,?CARRIERS, Id, AttachmentId, cb_context:req_verb(Context));
validate(Context,?MODB, Id, ?ATTACHMENT) ->
    validate_onbill(Context,?MODB, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_generate(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_generate(Context, DocsAccountId, ?HTTP_PUT) ->
    Year = kz_json:get_float_value(<<"year">>, cb_context:req_data(Context)),
    Month = kz_json:get_float_value(<<"month">>, cb_context:req_data(Context)),
    validate_generate(Context, DocsAccountId, Year, Month).

validate_generate(Context, DocsAccountId, Year, Month) when is_number(Year) andalso is_number(Month) ->
    case kz_json:get_value(<<"doc_type">>, cb_context:req_data(Context)) of
        "calls_reports" -> generate_per_minute_reports(Context, DocsAccountId, Year, Month);
     %  "calls_reports" -> generate_billing_docs(Context, DocsAccountId, Year, Month, 'per_minute_reports');
        _ -> maybe_generate_billing_docs(Context, DocsAccountId, Year, Month, 'generate_docs')
    end;

validate_generate(Context, _, _, _) ->
    Message = <<"Year and Month required">>,
    cb_context:add_validation_error(
      <<"Year and month">>
      ,<<"required">>
      ,kz_json:from_list([{<<"message">>, Message}])
      ,Context
     ).

maybe_generate_billing_docs(Context, DocsAccountId, Year, Month, FunName) ->
    case cb_context:is_superduper_admin(Context) of
        'true' ->
            generate_billing_docs(Context, DocsAccountId, Year, Month, FunName);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> generate_billing_docs(Context, DocsAccountId, Year, Month, FunName);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

maybe_spawn_generate_billing_docs(AccountId, Year, Month, FunName, N) ->
    case onbill_util:is_billable(AccountId) of
        'true' ->
            spawn(fun() ->
                      timer:sleep(N * 2000),
                      docs:FunName(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month))
                  end),
            N+1;
        'false' -> N
    end.

generate_billing_docs(Context, ?ALL_CHILDREN, Year, Month, FunName) ->
    ResellerId = cb_context:account_id(Context),
    case get_children_list(ResellerId) of
        {'ok', Accounts} ->
            _ = lists:foldl(fun(X, N) -> maybe_spawn_generate_billing_docs(kz_json:get_value(<<"id">>, X), Year, Month, FunName, N) end, 1, Accounts),
            cb_context:set_resp_status(Context, 'success');
        {'error', _Reason} ->
            cb_context:add_system_error('error', Context)
    end;
generate_billing_docs(Context, DocsAccountId, Year, Month, FunName) ->
    ResellerId = cb_context:account_id(Context),
    case validate_relationship(DocsAccountId, ResellerId) of
        'true' ->
            _ = docs:FunName(DocsAccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
            cb_context:set_resp_status(Context, 'success');
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

generate_per_minute_reports(Context, DocsAccountId, Year, Month) ->
    docs:per_minute_reports(DocsAccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    cb_context:set_resp_status(Context, 'success').

-spec validate_onbill(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill(Context, ?HTTP_GET) ->
    onbills_modb_summary(Context).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, ?RESELLER_VARIABLES, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_services:is_reseller(AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId) of
        'true' -> read_onbill(AccountId, Context);
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_onbill(Context, ?RESELLER_VARIABLES, ?HTTP_POST) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_services:is_reseller(AuthAccountId) orelse cb_context:is_superduper_admin(AuthAccountId) of
        'true' -> save_onbill(AccountId, Context);
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_onbill(Context, _Id, ?HTTP_GET) ->
    cb_context:add_system_error('only carrier or reseller docs are implemented for now...', Context).

validate_onbill(Context, ?CARRIERS, Id, ?HTTP_GET) ->
    read_onbill(<<"carrier.", (kz_util:to_binary(Id))/binary>>, Context);
validate_onbill(Context, ?CARRIERS, Id, ?HTTP_POST) ->
    save_onbill(build_carrier_doc_id(Id, Context), Context);
validate_onbill(Context, ?CUSTOMERS, Id, ?HTTP_GET) ->
    AccDoc = cb_context:account_doc(cb_context:set_account_id(Context, Id)),
    JObj = kz_json:get_value(<<"pvt_onbill">>, AccDoc),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:public_fields(JObj)}
                       ]);
validate_onbill(Context, ?CUSTOMERS, Id, ?HTTP_POST) ->
    AccDoc = cb_context:account_doc(cb_context:set_account_id(Context, Id)),
    JObj = kz_json:set_value(<<"pvt_onbill">>, cb_context:req_data(Context), AccDoc),
    crossbar_doc:save(cb_context:set_doc(Context, JObj));
validate_onbill(Context, ?SERVICE_PLANS, Id, ?HTTP_GET) ->
    crossbar_doc:load(Id, Context, [{'expected_type', <<"service_plan">>}]);
validate_onbill(Context, ?SERVICE_PLANS, Id, ?HTTP_POST) ->
    save_account(Id, <<"service_plan">>, Context).

validate_onbill(Context,?CARRIERS, Id, AttachmentId, ?HTTP_GET) ->
    load_carrier_attachment(Context, build_carrier_doc_id(Id, Context), <<Id/binary, "_", AttachmentId/binary, ".tpl">>);
validate_onbill(Context,?CARRIERS, Id, AttachmentId, ?HTTP_POST) ->
    save_carrier_attachment(Context, build_carrier_doc_id(Id, Context), <<Id/binary, "_", AttachmentId/binary, ".tpl">>);
validate_onbill(Context,?MODB, Id, ?ATTACHMENT, ?HTTP_GET) ->
    load_modb_attachment(Context, Id).

-spec read_onbill(ne_binary(), cb_context:context()) -> cb_context:context().
read_onbill(Id, Context) ->
    crossbar_doc:load(Id, cb_context:set_account_db(Context, <<"onbill">>)).

-spec save_onbill(ne_binary(), cb_context:context()) -> cb_context:context().
save_onbill(Id, Context) ->
    save(Id, <<"onbill">>, Context).

-spec save_account(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
save_account(Id, Type, Context) ->
    save(Id, kz_util:format_account_id(cb_context:account_id(Context),'encoded'), Type, Context).

-spec save(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
save(Id, DbName, Context) ->
    save(Id, DbName, 'undefined', Context).

-spec save(ne_binary(), ne_binary(), ne_binary()|atom(), cb_context:context()) -> cb_context:context().
save(Id, DbName, Type, Context) ->
    ReqData = kz_json:delete_key(<<"id">>, cb_context:req_data(Context)),
    Doc = case kz_datamgr:open_doc(DbName, Id) of
              {'ok', JObj} -> JObj;
              {error,not_found} ->
                  InitValues = props:filter_undefined([{<<"_id">>, Id}
                                                  ,{<<"pvt_type">>, Type}
                                                  ]),
                  kz_json:set_values(InitValues, kz_json:new())
          end,
    Values = kz_json:to_proplist(kz_doc:private_fields(Doc)),
    NewDoc = kz_json:set_values(Values, ReqData),
    Context1 = cb_context:set_doc(Context, NewDoc),
    crossbar_doc:save(cb_context:set_account_db(Context1, DbName)).

-spec onbills_modb_summary(cb_context:context()) -> cb_context:context().
onbills_modb_summary(Context) ->
    QueryString = cb_context:query_string(Context),
    {Year, Month} = case (kz_json:get_value(<<"year">>,QueryString) == 'undefined')
                          orelse
                          (kz_json:get_value(<<"month">>,QueryString) == 'undefined')
                    of
                        'true' ->
                            {{Y,M,_},_} = calendar:universal_time(),
                            {Y,M};
                        'false' ->
                            {kz_json:get_value(<<"year">>,QueryString), kz_json:get_value(<<"month">>,QueryString)}
                    end,
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    onbill_util:maybe_add_design_doc(Modb, <<"onbills">>),
    Context1 = cb_context:set_account_db(Context, Modb),
    crossbar_doc:load_view(?CB_LIST, [], Context1, fun onbill_util:normalize_view_results/2).

load_modb_attachment(Context0, Id) ->
    QueryString = cb_context:query_string(Context0),
    Year = kz_json:get_value(<<"year">>,QueryString),
    Month = kz_json:get_value(<<"month">>,QueryString),
    Context = crossbar_doc:load(Id, cb_context:set_account_modb(Context0, kz_util:to_integer(Year), kz_util:to_integer(Month))),
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    case onbill_util:get_attachment(Id, Modb) of
        {'ok', Attachment} ->
            cb_context:set_resp_etag(
                cb_context:set_resp_headers(cb_context:setters(Context,[{fun cb_context:set_resp_data/2, Attachment},{fun cb_context:set_resp_etag/2, 'undefined'}])
                                            ,[{<<"Content-Disposition">>, <<"attachment; filename="
                                                                            ,(kz_util:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(kz_util:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(kz_util:to_binary(Id))/binary>>
                                              }
                                             ,{<<"Content-Type">>, <<"application/pdf">>}
                                             |cb_context:resp_headers(Context)
                                           ])
                ,'undefined'
            );
        _ ->
            cb_context:add_system_error('faulty_request', Context0)
    end.

build_carrier_doc_id(Id, Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' ->
            <<"carrier.", (kz_util:to_binary(Id))/binary>>;
        'false' ->
            AccountId = cb_context:account_id(Context),
            <<"carrier.", (kz_util:to_binary(Id))/binary, ".", AccountId/binary>>
    end.

load_carrier_attachment(Context, DocId, AName) ->
    crossbar_doc:load_attachment(DocId, AName, [], cb_context:set_account_db(Context, <<"onbill">>)).

save_carrier_attachment(Context, DocId, AName) ->
    case cb_context:req_files(Context) of
        [{_FileName, FileJObj}] ->
            Contents = kz_json:get_value(<<"contents">>, FileJObj),
            CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
            crossbar_doc:save_attachment(
              DocId
              ,AName
              ,Contents
              ,cb_context:set_account_db(Context, <<"onbill">>)
              ,[{'content_type', kz_util:to_list(CT)}]
             );
        _ ->
            lager:debug("No file uploaded"),
            cb_context:add_system_error('no file uploaded', Context)
    end.

-spec validate_relationship(ne_binary(), ne_binary()) -> boolean().
validate_relationship(ChildId, ResellerId) ->
    case get_children_list(ResellerId) of
        {'ok', Accounts} ->
            AccountIds = lists:map(fun(Account) -> kz_json:get_value(<<"id">>, Account) end, Accounts),
            lists:member(ChildId, AccountIds);
        {'error', _Reason} = E ->
            lager:info("failed to load children. error: ~p", [E]),
            'false'
    end.

get_children_list(ResellerId) ->
    ViewOpts = [{'startkey', [ResellerId]}
               ,{'endkey', [ResellerId, kz_json:new()]}
               ],
    kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts).


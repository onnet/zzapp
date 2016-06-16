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
-define(MODB, <<"onbills_modb">>).
-define(RESELLER_VARIABLES, <<"onbill_reseller_variables">>).
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
allowed_methods(?GENERATE) ->
    [?HTTP_PUT];
allowed_methods(?RESELLER_VARIABLES) ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(?CARRIERS,_) ->
    [?HTTP_GET, ?HTTP_POST];
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
validate(Context, ?GENERATE) ->
    validate_generate(Context);
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).
validate(Context, ?CARRIERS, Id) ->
    validate_onbill(Context, ?CARRIERS, Id, cb_context:req_verb(Context)).
validate(Context,?CARRIERS, Id, AttachmentId) ->
    validate_onbill(Context,?CARRIERS, Id, AttachmentId, cb_context:req_verb(Context));
validate(Context,?MODB, Id, ?ATTACHMENT) ->
    validate_onbill(Context,?MODB, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_generate(cb_context:context()) -> cb_context:context().
validate_generate(Context) ->
    Year = kz_json:get_float_value(<<"year">>, cb_context:req_data(Context)),
    Month = kz_json:get_float_value(<<"month">>, cb_context:req_data(Context)),
    validate_generate(Context, Year, Month).

validate_generate(Context, Year, Month) when is_number(Year) andalso is_number(Month) ->
    case kz_json:get_value(<<"doc_type">>, cb_context:req_data(Context)) of
        "calls_reports" -> generate_per_minute_reports(Context, Year, Month);
        _ -> maybe_generate_billing_docs(Context, Year, Month)
    end;

validate_generate(Context, _, _) ->
    Message = <<"Year and Month required">>,
    cb_context:add_validation_error(
      <<"Year and month">>
      ,<<"required">>
      ,kz_json:from_list([{<<"message">>, Message}])
      ,Context
     ).

maybe_generate_billing_docs(Context, Year, Month) ->
    case cb_modules_util:is_superduper_admin(Context) of
        'true' ->
            generate_billing_docs(Context, Year, Month);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> generate_billing_docs(Context, Year, Month);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

generate_billing_docs(Context, Year, Month) ->
    AccountId = cb_context:account_id(Context),
    docs:generate_docs(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    cb_context:set_resp_status(Context, 'success').

generate_per_minute_reports(Context, Year, Month) ->
    AccountId = cb_context:account_id(Context),
    docs:per_minute_reports(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    cb_context:set_resp_status(Context, 'success').

-spec validate_onbill(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill(Context, ?HTTP_GET) ->
    onbills_modb_summary(Context).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, ?RESELLER_VARIABLES, ?HTTP_GET) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_services:is_reseller(AuthAccountId) orelse cb_modules_util:is_superduper_admin(AuthAccountId) of
        'true' -> read_onbill(AccountId, Context);
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_onbill(Context, ?RESELLER_VARIABLES, ?HTTP_POST) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_services:is_reseller(AuthAccountId) orelse cb_modules_util:is_superduper_admin(AuthAccountId) of
        'true' -> save_onbill(AccountId, Context);
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_onbill(Context, _Id, ?HTTP_GET) ->
    cb_context:add_system_error('only carrier or reseller docs are implemented for now...', Context).
%    read_onbill(Id, Context).

validate_onbill(Context, ?CARRIERS, Id, ?HTTP_GET) ->
    read_onbill(<<"carrier.", (kz_util:to_binary(Id))/binary>>, Context);
validate_onbill(Context, ?CARRIERS, Id, ?HTTP_POST) ->
    save_onbill(build_carrier_doc_id(Id, Context), Context).

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
    ReqData = kz_json:delete_key(<<"id">>, cb_context:req_data(Context)),
    Doc = case kz_datamgr:open_doc(<<"onbill">>, Id) of
              {'ok', JObj} -> JObj;
              {error,not_found} -> kz_json:new()
          end,
    Values = props:filter_undefined([{<<"_id">>, Id}
                                    ,{<<"_rev">>, kz_json:get_value(<<"_rev">>, Doc)}
                                    ,{<<"_attachments">>, kz_json:get_value(<<"_attachments">>, Doc)}
                                    ]),
    NewDoc = kz_json:set_values(Values, ReqData),
    Context1 = cb_context:set_doc(Context, NewDoc),
    crossbar_doc:save(cb_context:set_account_db(Context1, <<"onbill">>)).

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
    onbill_util:maybe_add_design_doc(Modb),
    Context1 = cb_context:set_account_db(Context, Modb),
    crossbar_doc:load_view(?CB_LIST, [], Context1, fun normalize_view_results/2).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

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
    case cb_modules_util:is_superduper_admin(Context) of
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


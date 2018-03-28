%%%-----------------------------------------------------------
%%%
%%% Saves/retrieves accounts/resellers variables to/from onbill doc.
%%% Variables are used for generation of accounting documents (papers)
%%% These variables are injected into template on each erlydtl doc generation,
%%% so each var added here could be used in doc template
%%%
%%%-----------------------------------------------------------

-module(cb_onbill_variables).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1, content_types_provided/2
         ,content_types_accepted/2
         ,validate/1, validate/2
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").
-include("onbill.hrl").

-define(TEMPLATE_NAME, <<"company_logo">>).
-define(MIME_TYPES, [{<<"image">>, <<"*">>},{<<"application">>, <<"pdf">>}]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_variables">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_variables">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_variables">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.onbill_variables">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_variables">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_) ->
    CTP = [{'to_binary', ?MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP).

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context,_) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?MIME_TYPES}
           ,{'from_json', ?JSON_CONTENT_TYPES}
          ],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbill(Context, cb_context:req_verb(Context)).
validate(Context, AttachmentId) ->
    validate_onbill(Context, AttachmentId, cb_context:req_verb(Context)).

-spec validate_onbill(cb_context:context(), http_method()) -> cb_context:context().
validate_onbill(Context, ?HTTP_GET) ->
    crossbar_doc:load(?VARIABLES_DOC_ID, Context, [{'expected_type', ?VARIABLES_DOC_TYPE}]);
validate_onbill(Context, ?HTTP_POST) ->
    save(?VARIABLES_DOC_ID, Context).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, AttachmentId, ?HTTP_GET) ->
    crossbar_doc:load_attachment(?VARIABLES_DOC_ID, AttachmentId, [{'expected_type', ?VARIABLES_DOC_TYPE}], Context);
validate_onbill(Context, AttachmentId, ?HTTP_POST) ->
    save_variables_attachment(Context, ?VARIABLES_DOC_ID, AttachmentId).

-spec save(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
save(Id, Context) ->
    ReqData = cb_context:req_data(Context),
    DbName = kz_util:format_account_id(cb_context:account_id(Context),'encoded'),
    NewDoc = case kz_datamgr:open_doc(DbName, Id) of
              {'ok', JObj} ->
                 Values = props:filter_undefined([{<<"_id">>, Id}
                                                 ,{<<"_attachments">>, kz_doc:attachments(JObj)}
                                                 ,{<<"_rev">>, kz_doc:revision(JObj)}
                                                 ]),
                 kz_json:set_values(Values, ReqData);
              {error,not_found} ->
                  kz_json:set_value(<<"_id">>, Id, kz_json:new())
          end,
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc), ?TYPE_CHECK_OPTION(<<"onbill">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            _ = replicate_onbill_doc_definition(Context1),
            Context1;
        _ -> Context1
    end.

save_variables_attachment(Context, DocId, AName) ->
    case cb_context:req_files(Context) of
        [{_FileName, FileJObj}] ->
            Contents = kz_json:get_value(<<"contents">>, FileJObj),
            CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
            crossbar_doc:save_attachment(
              DocId
              ,AName
              ,Contents
              ,Context
              ,[{'content_type', kz_term:to_list(CT)}]
             );
        _ ->
            lager:debug("No file uploaded"),
            cb_context:add_system_error('no file uploaded', Context)
    end.

-spec replicate_onbill_doc_definition(kz_json:object()) ->
                                          {'ok', kz_json:object()} |
                                          {'error', any()}.
replicate_onbill_doc_definition(Context) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    JObj0 = kz_json:set_value(<<"_id">>, AccountId, cb_context:doc(Context)),
    JObj = kz_json:delete_key(<<"_attachments">>, JObj0),
lager:info("IAM replicate_onbill_doc_definition JObj: ~p",[JObj]),
    DbName = ?ONBILL_DB(ResellerId),
    onbill_util:check_db(DbName),
    case kz_datamgr:lookup_doc_rev(DbName, AccountId) of
        {'ok', Rev} ->
lager:info("IAM replicate_onbill_doc_definition Rev: ~p",[Rev]),
            kz_datamgr:ensure_saved(DbName, kz_doc:set_revision(JObj, Rev));
        _Else ->
            kz_datamgr:ensure_saved(DbName, kz_doc:delete_revision(JObj))
    end.


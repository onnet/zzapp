%%%-----------------------------------------------------------
%%%
%%% Edit cariers docs
%%% Carriers docs intended to store carriers variables used upon documents generating, plus templates for these docs
%%%
%%%-----------------------------------------------------------

-module(cb_onbill_carriers).

-export([init/0
         ,allowed_methods/1, allowed_methods/2
         ,resource_exists/1, resource_exists/2
         ,content_types_provided/2, content_types_provided/3
         ,content_types_accepted/3
         ,validate/2, validate/3
        ]).

-include("/opt/kazoo/applications/crossbar/src/crossbar.hrl").

-define(CARRIER_DOC_TYPE, <<"onbill_carrier">>).
-define(TEMPLATE_NAME(Id, AttachmentId), <<Id/binary, "_", AttachmentId/binary, ".tpl">>).
-define(CARRIER_DOC_ID(Id), <<"onbill_carrier.", (kz_util:to_binary(Id))/binary>>).
-define(MIME_TYPES, [{<<"text">>, <<"html">>}]).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbill_carriers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbill_carriers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbill_carriers">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.onbill_carriers">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.onbill_carriers">>, ?MODULE, 'validate').

-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(_,_) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,_) -> 'true'.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context,_) ->
    Context.
content_types_provided(Context,_,_) ->
    CTP = [{'to_binary', ?MIME_TYPES}],
    cb_context:set_content_types_provided(Context, CTP).

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_accepted(Context,_,_) ->
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

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).
validate(Context, Id, AttachmentId) ->
    validate_onbill(Context, Id, AttachmentId, cb_context:req_verb(Context)).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, Id, ?HTTP_GET) ->
    crossbar_doc:load(?CARRIER_DOC_ID(Id), Context, [{'expected_type', ?CARRIER_DOC_TYPE}]);
validate_onbill(Context, Id, ?HTTP_POST) ->
    save(?CARRIER_DOC_ID(Id), Context).

-spec validate_onbill(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, Id, AttachmentId, ?HTTP_GET) ->
    crossbar_doc:load_attachment(?CARRIER_DOC_ID(Id), ?TEMPLATE_NAME(Id, AttachmentId), [], Context);
validate_onbill(Context, Id, AttachmentId, ?HTTP_POST) ->
    save_carrier_attachment(Context, ?CARRIER_DOC_ID(Id), ?TEMPLATE_NAME(Id, AttachmentId)).

-spec save(ne_binary(), cb_context:context()) -> cb_context:context().
save(Id, Context) ->
    ReqData = cb_context:req_data(Context),
    DbName = kz_util:format_account_id(cb_context:account_id(Context),'encoded'),
    Doc = case kz_datamgr:open_doc(DbName, Id) of
              {'ok', JObj} ->
                  JObj;
              {error,not_found} ->
                  kz_json:set_value(<<"_id">>, Id, kz_json:new())
          end,
    NewDoc = kz_json:merge_recursive(Doc, ReqData),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    cb_context:set_resp_data(Context1, ReqData).

save_carrier_attachment(Context, DocId, AName) ->
    case cb_context:req_files(Context) of
        [{_FileName, FileJObj}] ->
            Contents = kz_json:get_value(<<"contents">>, FileJObj),
            CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
            crossbar_doc:save_attachment(
              DocId
              ,AName
              ,Contents
              ,Context
              ,[{'content_type', kz_util:to_list(CT)}]
             );
        _ ->
            lager:debug("No file uploaded"),
            cb_context:add_system_error('no file uploaded', Context)
    end.

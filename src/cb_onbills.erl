-module(cb_onbills).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("../../crossbar/src/crossbar.hrl").

-define(CB_LIST, <<"onbills/crossbar_listing">>).
-define(ATTACHMENT, <<"attachment">>).
-define(GENERATE, <<"generate">>).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.onbills">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.onbills">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.onbills">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.onbills">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_,?ATTACHMENT) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,?ATTACHMENT) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_,?ATTACHMENT) ->
    CTP = [{'to_binary', [{<<"application">>, <<"pdf">>}]}],
    cb_context:set_content_types_provided(Context, CTP).

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbills(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).
validate(Context, Id, ?ATTACHMENT) ->
    validate_onbill(Context, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_onbills(cb_context:context(), http_method()) -> cb_context:context().
validate_onbills(Context, ?HTTP_GET) ->
    summary(Context).

-spec validate_onbill(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_onbill(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

validate_onbill(Context, Id, ?ATTACHMENT, ?HTTP_GET) ->
    load_attachment(Context, Id).

-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    QueryString = cb_context:query_string(Context),
    {Year, Month} = case (wh_json:get_value(<<"year">>,QueryString) == 'undefined')
                          orelse
                          (wh_json:get_value(<<"month">>,QueryString) == 'undefined')
                    of
                        'true' ->
                            {{Y,M,_},_} = calendar:universal_time(),
                            {Y,M};
                        'false' ->
                            {wh_json:get_value(<<"year">>,QueryString), wh_json:get_value(<<"month">>,QueryString)}
                    end,
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    onbill_util:maybe_add_design_doc(Modb),
    Context1 = cb_context:set_account_db(Context, Modb),
    crossbar_doc:load_view(?CB_LIST, [], Context1, fun normalize_view_results/2).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

load_attachment(Context0, Id) ->
    QueryString = cb_context:query_string(Context0),
    Year = wh_json:get_value(<<"year">>,QueryString),
    Month = wh_json:get_value(<<"month">>,QueryString),
    Context = crossbar_doc:load(Id, cb_context:set_account_modb(Context0, wh_util:to_integer(Year), wh_util:to_integer(Month))),
    AccountId = cb_context:account_id(Context),
    Modb = kazoo_modb:get_modb(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    case onbill_util:get_attachment(Id, Modb) of
        {'ok', Attachment} ->
            cb_context:set_resp_etag(
                cb_context:set_resp_headers(cb_context:setters(Context,[{fun cb_context:set_resp_data/2, Attachment},{fun cb_context:set_resp_etag/2, 'undefined'}])
                                            ,[{<<"Content-Disposition">>, <<"attachment; filename="
                                                                            ,(wh_util:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(wh_util:to_binary(Id))/binary
                                                                            ,"-"
                                                                            ,(wh_util:to_binary(Id))/binary>>
                                              }
                                             ,{<<"Content-Type">>, <<"application/pdf">>}
                                             |cb_context:resp_headers(Context)
                                           ])
                ,'undefined'
            );
        _ ->
         %   crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context)
            cb_context:add_system_error('faulty_request', Context0)
    end.

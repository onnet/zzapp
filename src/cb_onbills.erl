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
allowed_methods(?GENERATE) ->
    [?HTTP_PUT];
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_,?ATTACHMENT) ->
    [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_,?ATTACHMENT) -> 'true';
resource_exists(_,?GENERATE) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.
content_types_provided(Context,_,?ATTACHMENT) ->
    CTP = [{'to_binary', [{<<"application">>, <<"pdf">>}]}],
    cb_context:set_content_types_provided(Context, CTP);
content_types_provided(Context,_,?GENERATE) ->
    Context.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_onbills(Context, cb_context:req_verb(Context)).
validate(Context, ?GENERATE) ->
    validate_generate(Context);
validate(Context, Id) ->
    validate_onbill(Context, Id, cb_context:req_verb(Context)).
validate(Context, Id, ?ATTACHMENT) ->
    validate_onbill(Context, Id, ?ATTACHMENT, cb_context:req_verb(Context)).

-spec validate_generate(cb_context:context()) -> cb_context:context().
validate_generate(Context) ->
    Year = kz_json:get_float_value(<<"year">>, cb_context:req_data(Context)),
    Month = kz_json:get_float_value(<<"month">>, cb_context:req_data(Context)),

    case cb_modules_util:is_superduper_admin(Context) of
        'true' -> validate_generate(Context, Year, Month);
        'false' ->
            case kz_services:is_reseller(cb_context:auth_account_id(Context)) of
                'true' -> validate_generate(Context, Year, Month);
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end.

validate_generate(Context, Year, Month) when Year == 'undefined' orelse Month == 'undefined' ->
    Message = <<"Year and Month required">>,
    cb_context:add_validation_error(
      <<"Year and month">>
      ,<<"required">>
      ,kz_json:from_list([{<<"message">>, Message}])
      ,Context
     );

validate_generate(Context, Year, Month) when is_float(Year) orelse is_float(Month) ->
    validate_generate(Context, kz_util:to_integer(Year), kz_util:to_integer(Month));

validate_generate(Context, Year, Month) ->
    AccountId = cb_context:account_id(Context),
    fees_and_docs:generate_docs(AccountId, Year, Month),
    cb_context:set_resp_status(Context, 'success').

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

load_attachment(Context0, Id) ->
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
         %   crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context)
            cb_context:add_system_error('faulty_request', Context0)
    end.

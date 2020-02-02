%%
%%  https://crossbar_server_with_443_port/hooks/bitrix_auth/5c36900d63069782e4e0b0ad2f131e45
%%  

-module(cb_bitrix_auth).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,authenticate/1
        ,authorize/1
        ,validate/1, validate/2
        ]).

-define(CATEGORY, <<"bitrix_auth">>).

-include_lib("crossbar/src/crossbar.hrl").
-include_lib("onbill.hrl").

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.bitrix_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.bitrix_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.bitrix_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.bitrix_auth">>, ?MODULE, 'post'),
    ok.

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_AccountId) -> [?HTTP_GET].

-spec resource_exists() -> boolean().
resource_exists() -> 'true'.

-spec resource_exists(path_tokens()) -> boolean().
resource_exists(?MATCH_ACCOUNT_RAW(_AccountId)) -> 'true';
resource_exists(_) -> 'false'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    lager:info("CB_Bitrix authenticate cb_context:req_nouns: ~p",[cb_context:req_nouns(Context)]),
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"bitrix_auth">>, []}]) -> 'true';
authenticate_nouns([{<<"bitrix_auth">>, [_]}]) -> 'true';
authenticate_nouns(_Nouns) -> 'false'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    lager:info("CB_Bitrix authorize cb_context:req_nouns: ~p",[cb_context:req_nouns(Context)]),
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"bitrix_auth">>, []}]) -> 'true';
authorize_nouns([{<<"bitrix_auth">>, [_]}]) -> 'true';
authorize_nouns(_Nouns) -> 'false'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    lager:info("CB_Bitrix validate cb_context:query_string(Context): ~p",[cb_context:query_string(Context)]),
    QueryString = cb_context:query_string(Context),
    case kz_json:get_value(<<"state">>, QueryString) of
        <<AccountId:32/binary, ",", HookId:32/binary, ",", AuthToken/binary>> ->
            case kz_auth_jwt:decode(AuthToken) of
                {ok, _, _Proplist} ->
                    lager:info("CB_Bitrix validate AccountId: ~p",[AccountId]),
                    lager:info("CB_Bitrix validate HookId: ~p",[HookId]),
                    lager:info("CB_Bitrix validate AuthToken: ~p",[AuthToken]),
                    lager:info("CB_Bitrix validate query_string: ~p",[QueryString]),
                    {'ok', HookJObj} = kz_datamgr:open_doc(?KZ_WEBHOOKS_DB, HookId),
                    ClientSecret = kz_json:get_value([<<"custom_data">>,<<"client_secret">>], HookJObj),
                    lager:info("CB_Bitrix validate ClientSecret: ~p",[ClientSecret]),
         %           Ctx1 = cb_context:set_resp_data(Context, QueryString),
         %           Ctx3 = cb_context:set_resp_header(Ctx2, <<"content-type">>, <<"text/html">>),
         %           Ctx3 = cb_context:set_resp_headers(Ctx2, [{<<"content-type">>, <<"text/html">>}, {<<"location">>, <<"https://cisco.com">>}]),
         %           Ctx4 =  cb_context:add_resp_header(Ctx3, <<"location">>, <<"https://cisco.com">>),
         %           crossbar_util:response_redirect(Ctx3, <<"https://cisco.com">>, kz_json:new(), 302);
         %           Ctx4;
    Headers = #{<<"content-type">> => <<"text/html">>
               ,<<"location">> => <<"https://cisco.com">>
               },
  %  Ctx = cb_context:set_resp_headers(Context, Headers),
    Ctx = cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'error'}
                       ,{fun cb_context:set_resp_error_msg/2, <<"redirect">>}
                       ,{fun cb_context:set_resp_error_code/2, 302}
             %          ,{fun cb_context:set_resp_data/2, JTerm}
                       ]),
    cb_context:set_resp_headers(Ctx, Headers);
                _ ->
                    crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Invalid AuthToken">>}]}, Context)
            end;
        _ ->
            crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Invalid state">>}]}, Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, BitrixIntegrationAccountId) ->
    case kz_auth_jwt:decode(cb_context:auth_token(Context)) of
        {ok,_,Proplist} ->
            AuthTokenAccountId = props:get_value(<<"account_id">>, Proplist),
            ResellerId = zz_util:find_reseller_id(BitrixIntegrationAccountId),
            case AuthTokenAccountId == BitrixIntegrationAccountId 
                 orelse AuthTokenAccountId == ResellerId
            of
                'true' ->
                    BitrixClientID = kapps_account_config:get(ResellerId
                                                             ,<<"bitrix">>
                                                             ,<<"client_id">>
                                                             ,<<"no__client_id__configured">>
                                                             ),
                    Ctx1 = cb_context:set_resp_data(Context, kz_json:set_value(<<"client_id">>, BitrixClientID, kz_json:new())),
                    cb_context:set_resp_status(Ctx1, 'success');
                'false' ->
                    crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Invalid data">>}]}, Context)
            end;
        'false' ->
            lager:info("CB_Bitrix query_string: ~p",[cb_context:query_string(Context)]),
            crossbar_util:response('error', <<"error">>, 400,{[{<<"message">>, <<"Non valid token">>}]}, Context)
    end.

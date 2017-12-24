-module(lb_http).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

%% interface functions
-export([
     lb_login/1
    ,lb_logout/1
    ,add_payment/5
    ,maybe_unblock_agreement/2
    ,lb_soap_get_uid_by_number/2
]).

-include_lib("onbill.hrl").

-define(SOAPENV_O, "<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/' xmlns:urn='urn:api3'>").
-define(SOAPENV_C, "</soapenv:Envelope>").
-define(BODY_O, "<soapenv:Body>").
-define(BODY_C, "</soapenv:Body>").
-define(ENCTYPE_XML, "text/xml").
-define(LB_CONF(Var, AccountId)
       ,kz_term:to_list(kz_json:get_binary_value([<<"mod_lb">>, Var], onbill_util:account_vars(AccountId), <<"not_found">>))
       ).

-spec lb_login(ne_binary()) -> any().
lb_login(AccountId) ->
    EncType = "application/x-www-form-urlencoded",
    Url = ?LB_CONF(<<"lb_url">>, AccountId),
    LB_User = ?LB_CONF(<<"lb_user">>, AccountId),
    LB_Password = ?LB_CONF(<<"lb_password">>, AccountId),
    AuthStr = io_lib:format("login=~s&password=~s",[LB_User, LB_Password]),
    httpc:set_options([{cookies, enabled}]),
    httpc:request(post, {Url, [], EncType, lists:flatten(AuthStr)}, [], []).

-spec lb_logout(ne_binary()) -> any().
lb_logout(AccountId) ->
    EncType = "application/x-www-form-urlencoded",
    Url = ?LB_CONF(<<"lb_url">>, AccountId),
    httpc:request(post, {Url, [], EncType, lists:flatten("devision=99")}, [], []).

-spec lb_soap_auth(atom(), ne_binary()) -> any().
lb_soap_auth(Move, AccountId) ->
    Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
    case Move of
        'login' ->
            LB_User = ?LB_CONF(<<"lb_user">>, AccountId),
            LB_Password = ?LB_CONF(<<"lb_password">>, AccountId),
            httpc:set_options([{cookies, enabled}]),
            httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_login_data(LB_User, LB_Password)}, [], []);
        'logout' ->
            httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_logout_data()}, [], [])
    end.

-spec maybe_unblock_agreement(ne_binary(), any()) -> any().
maybe_unblock_agreement(AgrmId, AccountId) ->
    timer:sleep(300000),
    case re:run(lb_get_balance_by_agrmid(AgrmId, AccountId),"-") of
        nomatch ->
            lb_util:set_notify_balance_by_agrmid(500, AgrmId, AccountId),
            lb_block_agreement(AgrmId, 10, "off", AccountId);
        {match,[{0,1}]} -> 'ok'
    end.

lb_block_agreement(AgrmId, BlkType, State, AccountId) ->
    Xml_VgidList = lb_soap_getVgroups(AgrmId, BlkType, AccountId),
    lb_soap_block_Vgroups(Xml_VgidList, BlkType, State, AccountId).

lb_soap_blkVgroup(Id, BlkType, State, AccountId) ->
    lb_soap_auth('login', AccountId),
    Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
    httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_blkVgroup_data(Id, BlkType, State)}, [], []),
    lb_soap_auth('logout', AccountId).

lb_soap_getVgroups(AgrmId, BlkType, AccountId) ->
    lb_soap_auth('login', AccountId),
    Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_getVgroups_data(AgrmId, BlkType)}, [], []),
    lb_soap_auth('logout', AccountId),
    {Xml, _} = xmerl_scan:string(Body),
    xmerl_xpath:string("//SOAP-ENV:Envelope/SOAP-ENV:Body/lbapi:getVgroupsResponse/ret/vgid/text()", Xml).

-spec lb_soap_get_uid_by_number(ne_binary(), any()) -> any().
lb_soap_get_uid_by_number(PhoneNumber, AccountId) ->
    try
      Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
      lb_soap_auth('login', AccountId),
      {ok, {{_, 200, _}, _, Body}} = httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_getVgroups_filter("<phone>"++z_convert:to_list(PhoneNumber)++"</phone>")}, [], []),
      lb_soap_auth('logout', AccountId),
      {Xml, _} = xmerl_scan:string(Body),
      L = xmerl_xpath:string("//SOAP-ENV:Envelope/SOAP-ENV:Body/lbapi:getVgroupsResponse/ret/uid/text()", Xml),
      [H|_] = lists:reverse(L),
 %     [H|_] = L,
      [[H#xmlText.value]]
    catch
      error:_ -> []
    end.

lb_soap_block_Vgroups([], _, _, _) ->
    ok;
lb_soap_block_Vgroups([H|T], BlkType, State, AccountId) ->
    lb_soap_block_Vgroup(H, BlkType, State, AccountId),
    lb_soap_block_Vgroups(T, BlkType, State, AccountId).

lb_soap_block_Vgroup(XmlRec, BlkType, State, AccountId) ->
    lb_soap_blkVgroup(XmlRec#xmlText.value, BlkType, State, AccountId).

lb_soap_getAgreement(AgrmId, AccountId) ->
    lb_soap_auth('login', AccountId),
    Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_getAgreements_data(AgrmId)}, [], []),
    lb_soap_auth('logout', AccountId),
    Body.

lb_get_balance_by_agrmid(AgrmId, AccountId) ->
    Body = lb_soap_getAgreement(AgrmId, AccountId),
    {Xml, _} = xmerl_scan:string(Body),
    [H|_] = xmerl_xpath:string("//SOAP-ENV:Envelope/SOAP-ENV:Body/lbapi:getAgreementsResponse/ret/balance/text()", Xml),
    H#xmlText.value.

-spec add_payment(any(), any(), any(), any(), any()) -> any().
add_payment(Agrm_Id, Summ, Receipt, Comment, AccountId) ->
    Url = ?LB_CONF(<<"lb_url">>, AccountId),
    QueryString = io_lib:format("async_call=1&devision=199&commit_payment=~s&payment_sum=~s&payment_number=~s&classid=0&_paymentType=zonnet_add_payment&payment_comment=~s", [Agrm_Id, Summ, Receipt, Comment]),
    EncType = "application/x-www-form-urlencoded",
    lb_login(AccountId),
    _ = httpc:request(post, {Url, [], EncType, lists:flatten(QueryString)}, [], []),
    _ = spawn(?MODULE, 'maybe_unblock_agreement', [Agrm_Id, AccountId]),
    lb_logout(AccountId).

lb_login_data(Login, Password) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:Login>"
          ++ "<login>"++z_convert:to_list(Login)++"</login>"
          ++ "<pass>"++z_convert:to_list(Password)++"</pass>"
        ++ "</urn:Login>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_blkVgroup_data(Id, Blk, State) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:blkVgroup>"
          ++ "<id>"++z_convert:to_list(Id)++"</id>"
          ++ "<blk>"++z_convert:to_list(Blk)++"</blk>"
          ++ "<state>"++z_convert:to_list(State)++"</state>"
        ++ "</urn:blkVgroup>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_logout_data() ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:Logout/>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_getVgroups_data(AgrmId, BlockedId) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:getVgroups>"
          ++ "<flt>"
            ++ "<agrmid>"++z_convert:to_list(AgrmId)++"</agrmid>"
            ++ "<blocked>"++z_convert:to_list(BlockedId)++"</blocked>"
          ++ "</flt>"
        ++ "</urn:getVgroups>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_getVgroups_filter(FilterString) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:getVgroups>"
          ++ "<flt>"
            ++ FilterString
          ++ "</flt>"
        ++ "</urn:getVgroups>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_getAgreements_data(AgrmId) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:getAgreements>"
          ++ "<flt>"
            ++ "<agrmid>"++z_convert:to_list(AgrmId)++"</agrmid>"
          ++ "</flt>"
        ++ "</urn:getAgreements>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.


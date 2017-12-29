-module(lb_http).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([
     lb_login/1
    ,lb_logout/1
    ,add_payment/5
    ,soap_create_account/4
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
       ,kz_term:to_list(kz_json:get_binary_value([<<"mod_lb">>, Var], onbill_util:reseller_vars(AccountId), <<"not_found">>))
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
      {ok, {{_, 200, _}, _, Body}} = httpc:request(post, {Url, [], ?ENCTYPE_XML, lb_getVgroups_filter("<phone>"++kz_term:to_list(PhoneNumber)++"</phone>")}, [], []),
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
add_payment(AgrmId, Summ, Receipt, Comment, AccountId) ->
    Url = ?LB_CONF(<<"lb_url">>, AccountId),
    QueryString = "async_call=1"
               ++ "&devision=199"
               ++ "&commit_payment=" ++ kz_term:to_list(AgrmId)
               ++ "&payment_sum=" ++ kz_term:to_list(Summ)
               ++ "&payment_number=" ++ kz_term:to_list(Receipt)
               ++ "&classid=0"
               ++ "&_paymentType=zonnet_add_payment"
               ++ "&payment_comment=" ++ kz_term:to_list(Comment),
    EncType = "application/x-www-form-urlencoded",
    lb_login(AccountId),
    _ = httpc:request(post, {Url, [], EncType, lists:flatten(QueryString)}, [], []),
    _ = spawn(?MODULE, 'maybe_unblock_agreement', [AgrmId, AccountId]),
    lb_logout(AccountId).

-spec soap_create_account(any(), any(), any(), any()) -> any().
soap_create_account(AccountId, Login, Passwd, Type) ->
    XML_Data = 
    ?SOAPENV_O
      ++ ?BODY_O
      ++ "<urn:insupdAccount>"
      ++  "<isInsert>1</isInsert>"
        ++ "<val>"
            ++ "<account>"
                ++ "<uid></uid>"
                ++ "<uuid>"++kz_term:to_list(AccountId)++"</uuid>"
                ++ "<ipaccess>0</ipaccess>"
                ++ "<billdelivery>0</billdelivery>"
                ++ "<category>0</category>"
                ++ "<type>"++kz_term:to_integer(Type)++"</type>"
                ++ "<oksm>0</oksm>"
                ++ "<templ>0</templ>"
                ++ "<wrongactive>0</wrongactive>"
                ++ "<archive>0</archive>"
                ++ "<login>"++kz_term:to_list(Login)++"</login>"
                ++ "<pass>"++kz_term:to_list(Passwd)++"</pass>"
                ++ "<descr></descr>"
                ++ "<name>"++kz_term:to_list(Login)++"</name>"
                ++ "<phone></phone>"
                ++ "<fax></fax>"
                ++ "<email></email>"
                ++ "<bankname></bankname>"
                ++ "<branchbankname></branchbankname>"
                ++ "<treasuryname></treasuryname>"
                ++ "<treasuryaccount></treasuryaccount>"
                ++ "<bik></bik>"
                ++ "<settl></settl>"
                ++ "<corr></corr>"
                ++ "<kpp></kpp>"
                ++ "<inn></inn>"
                ++ "<ogrn></ogrn>"
                ++ "<okpo></okpo>"
                ++ "<okved></okved>"
                ++ "<gendiru></gendiru>"
                ++ "<glbuhgu></glbuhgu>"
                ++ "<kontperson></kontperson>"
                ++ "<actonwhat></actonwhat>"
                ++ "<passsernum></passsernum>"
                ++ "<passno></passno>"
                ++ "<passissuedate></passissuedate>"
                ++ "<passissuedep></passissuedep>"
                ++ "<passissueplace></passissueplace>"
                ++ "<birthdate></birthdate>"
                ++ "<birthplace></birthplace>"
                ++ "<lastmoddate></lastmoddate>"
                ++ "<wrongdate></wrongdate>"
                ++ "<okato></okato>"
            ++ "</account>"
            ++ "<usergroups>"
                ++ "<usergroup>"
                    ++ "<groupid>0</groupid>"
                    ++ "<promiseallow>0</promiseallow>"
                    ++ "<promiserent>0</promiserent>"
                    ++ "<promisetill>0</promisetill>"
                    ++ "<promisemax>0.0</promisemax>"
                    ++ "<promisemin>0.0</promisemin>"
                    ++ "<promiselimit>0.0</promiselimit>"
                    ++ "<name></name>"
                    ++ "<description></description>"
                ++ "</usergroup>"
                ++ "<usercnt>0</usercnt>"
                ++ "<fread>0</fread>"
                ++ "<fwrite>0</fwrite>"
            ++ "</usergroups>"
            ++ "<addresses>"
                ++ "<type>0</type>"
                ++ "<code></code>"
                ++ "<address></address>"
            ++ "</addresses>"
            ++ "<agreements>"
                ++ "<agrmid></agrmid>"
                ++ "<uid>0</uid>"
                ++ "<operid>0</operid>"
                ++ "<curid>1</curid>"
                ++ "<bnotify>0</bnotify>"
                ++ "<archive>0</archive>"
                ++ "<vgroups>0</vgroups>"
                ++ "<balance>0.0</balance>"
                ++ "<balancecurr>0.0</balancecurr>"
                ++ "<credit>0.0</credit>"
                ++ "<blimit>0.0</blimit>"
                ++ "<number></number>"
                ++ "<code></code>"
                ++ "<date></date>"
                ++ "<bcheck></bcheck>"
                ++ "<symbol></symbol>"
            ++ "</agreements>"
        ++ "</val>"
      ++ "</urn:insupdAccount>"
      ++ ?BODY_C
    ++ ?SOAPENV_C,
    lb_soap_auth('login', AccountId),
    Url = ?LB_CONF(<<"lb_soap_url">>, AccountId),
    _ = httpc:request(post, {Url, [], ?ENCTYPE_XML, XML_Data}, [], []),
    lb_soap_auth('logout', AccountId).

lb_login_data(Login, Password) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:Login>"
          ++ "<login>"++kz_term:to_list(Login)++"</login>"
          ++ "<pass>"++kz_term:to_list(Password)++"</pass>"
        ++ "</urn:Login>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.

lb_blkVgroup_data(Id, Blk, State) ->
    ?SOAPENV_O
      ++ ?BODY_O
        ++ "<urn:blkVgroup>"
          ++ "<id>"++kz_term:to_list(Id)++"</id>"
          ++ "<blk>"++kz_term:to_list(Blk)++"</blk>"
          ++ "<state>"++kz_term:to_list(State)++"</state>"
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
            ++ "<agrmid>"++kz_term:to_list(AgrmId)++"</agrmid>"
            ++ "<blocked>"++kz_term:to_list(BlockedId)++"</blocked>"
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
            ++ "<agrmid>"++kz_term:to_list(AgrmId)++"</agrmid>"
          ++ "</flt>"
        ++ "</urn:getAgreements>"
      ++ ?BODY_C
    ++ ?SOAPENV_C.


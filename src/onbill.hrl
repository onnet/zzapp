-ifndef(ONBILL_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(APP, 'onbill').
-define(OB_APP_NAME, <<"onbill">>).
-define(OB_APP_VERSION, <<"4.0.0">> ).

-define(SERVICES_DB, <<"services">>).
-define(ONBILL_DB, <<"onbill">>).
-define(ONBILL_DOC, <<"onbill">>).
-define(MRC_DOC, <<"monthly_recurring">>).
-define(TO_BIN(Var), kz_term:to_binary(Var)).
-define(TO_INT(Var), kz_term:to_integer(Var)).
-define(TO_FLT(Var), kz_term:to_float(Var)).
-define(CARRIER_DOC(CarrierName), <<"onbill_carrier.", CarrierName/binary>>).
-define(MOD_CONFIG_CRAWLER, <<(?OB_APP_NAME)/binary, ".account_crawler">>).
-define(DOCS_NUMBER_DB(ResellerId, Year)
       ,<<(?OB_APP_NAME)/binary
         ,"-"
         ,ResellerId/binary
         ,"-"
         ,(?TO_BIN(Year))/binary>>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(DOC_NAME_FORMAT(Carrier, TemplateID), <<Carrier/binary, "_", TemplateId/binary>>).
-define(ONBILL_DOC_ID_FORMAT(Year, Month, Carrier, TemplateId)
       ,<<(?TO_BIN(Year))/binary
         ,(kz_time:pad_month(Month))/binary
         ,"-"
         ,Carrier/binary
         ,"_"
         ,TemplateId/binary>>).
-define(ONBILL_DOC_ID_FORMAT(Year, Month, DocNumber, Carrier, TemplateId)
       ,<<(?TO_BIN(Year))/binary
         ,(kz_time:pad_month(Month))/binary
         ,"-"
         ,(?TO_BIN(DocNumber))/binary
         ,"-"
         ,Carrier/binary
         ,"_"
         ,TemplateId/binary>>).
-define(HTML_TO_PDF, <<"/usr/local/bin/wkhtmltopdf --quiet">>).
-define(DEFAULT_REGEX, <<"^\\d*$">>).
-define(DATE_STRING(Year, Month, Day)
       ,<<(kz_time:pad_day(Day))/binary, ".",(kz_time:pad_month(Month))/binary,".",(?TO_BIN(Year))/binary>>).
-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).
-define(MRC_TEMPLATE, <<"customer_update_mrc">>).
-define(MRC_APPROACHING_TEMPLATE, <<"customer_update_mrc_approaching">>).
-define(LIMITS_SET_TO_ZERO_TEMPLATE, <<"customer_update_limits_set_to_zero">>).
-define(SERVICE_SUSPENDED_TEMPLATE, <<"customer_update_service_suspended">>).
-define(TRIAL_HAS_EXPIRED_TEMPLATE, <<"customer_update_trial_has_expired">>).
-define(DAILY_FEE_DOC_NAME(Month, Year, Day)
       ,<<(?TO_BIN(Year))/binary, (kz_time:pad_month(Month))/binary, (kz_time:pad_day(Day))/binary,  "-dailyfee">>).
-define(BEGIN_DAY_TS(Month, Year, Day), calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{0,0,0}})).
-define(END_DAY_TS(Month, Year, Day), calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{23,59,59}})).

-define(ONBILL_HRL, 'true').
-endif.

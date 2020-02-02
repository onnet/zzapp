-ifndef(ONBILL_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").
-include_lib("kazoo/include/kz_api_literals.hrl").

%%-define(APP, 'zzapp').
-define(ZZ_APP_NAME, <<"zzapp">>).
-define(ZZ_APP_VERSION, <<"4.0.0">> ).

-define(SERVICES_DB, <<"services">>).
-define(ONBILL_DOC, <<"onbill">>).
-define(VARIABLES_DOC_TYPE, <<"onbill">>).
-define(VARIABLES_DOC_ID, <<"onbill">>).
-define(MRC_DOC, <<"monthly_recurring">>).
-define(TO_BIN(Var), kz_term:to_binary(Var)).
-define(TO_INT(Var), kz_term:to_integer(Var)).
-define(TO_FLT(Var), kz_term:to_float(Var)).
-define(CARRIER_DOC(CarrierName), <<"onbill_carrier.", CarrierName/binary>>).
-define(MOD_CONFIG_CRAWLER, <<(?ZZ_APP_NAME)/binary, ".account_crawler">>).
-define(ONBILL_DB(ResellerId), <<(?ZZ_APP_NAME)/binary, "-", ResellerId/binary>>).
-define(DOCS_NUMBER_DB(ResellerId, Year)
       ,<<(?ZZ_APP_NAME)/binary
         ,"-"
         ,ResellerId/binary
         ,"-"
         ,(?TO_BIN(Year))/binary>>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(DOC_NAME_FORMAT(Carrier, TemplateID), <<Carrier/binary, "_", TemplateId/binary>>).
-define(ONBILL_DOC_ID_FORMAT(Year, Month, Carrier, TemplateId)
       ,<<(?TO_BIN(Year))/binary
         ,(kz_date:pad_month(Month))/binary
         ,"-"
         ,Carrier/binary
         ,"_"
         ,TemplateId/binary>>).
-define(ONBILL_DOC_ID_FORMAT(Year, Month, DocNumber, Carrier, TemplateId)
       ,<<(?TO_BIN(Year))/binary
         ,(kz_date:pad_month(Month))/binary
         ,"-"
         ,(?TO_BIN(DocNumber))/binary
         ,"-"
         ,Carrier/binary
         ,"_"
         ,TemplateId/binary>>).
-define(HTML_TO_PDF(WkhtmlOptions), <<"/usr/local/bin/wkhtmltopdf --quiet ", WkhtmlOptions/binary>>).
-define(WKHTMLTOPDF, <<"wkhtmltopdf">>).
-define(DEFAULT_REGEX, <<"^\\d*$">>).
-define(DATE_STRING(Year, Month, Day)
       ,<<(kz_date:pad_day(Day))/binary, ".",(kz_date:pad_month(Month))/binary,".",(?TO_BIN(Year))/binary>>).
-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).
-define(MRC_TEMPLATE, <<"customer_update_mrc">>).
-define(MRC_APPROACHING_TEMPLATE, <<"customer_update_mrc_approaching">>).
-define(LIMITS_SET_TO_ZERO_TEMPLATE, <<"customer_update_limits_set_to_zero">>).
-define(SERVICE_SUSPENDED_TEMPLATE, <<"customer_update_service_suspended">>).
-define(TRIAL_HAS_EXPIRED_TEMPLATE, <<"customer_update_trial_has_expired">>).
-define(DAILY_FEE_DOC_NAME(Month, Year, Day)
       ,<<(?TO_BIN(Year))/binary, (kz_date:pad_month(Month))/binary, (kz_date:pad_day(Day))/binary,  "-dailyfee">>).
-define(BEGIN_DAY_TS(Month, Year, Day), calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{0,0,0}})).
-define(END_DAY_TS(Month, Year, Day), calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{23,59,59}})).
-define(PERIOD_DOCS_VIEW, <<"onbills/docs_by_period_ts">>).
-define(CB_LIST, <<"onbills/crossbar_listing">>).
-define(PAUSE, 500).

%%%%%%%%%%%%%%%%%%%%%%%%%% TMP for 4.3 move %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%  from core/kazoo_transactions/include/kazoo_transactions.hrl %%%%%%%%%%%%%%%%%

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type units() :: non_neg_integer().
-type dollars() :: number().

-define(TOPUP_CONFIG, <<"topup">>).

-define(CODE_PER_MINUTE_CALL, 1001).
-define(CODE_SUB_ACCOUNT_PER_MINUTE_CALL, 1002).
-define(CODE_FEATURE_ACTIVATION, 2001).
-define(CODE_SUB_ACCOUNT_FEATURE_ACTIVATION, 2002).
-define(CODE_NUMBER_ACTIVATION, 2003).
-define(CODE_SUB_ACCOUNT_NUMBER_ACTIVATION, 2004).
-define(CODE_NUMBERS_ACTIVATION, 2005).
-define(CODE_SUB_ACCOUNT_NUMBERS_ACTIVATION, 2006).
-define(CODE_MANUAL_ADDITION, 3001).
-define(CODE_SUB_ACCOUNT_MANUAL_ADDITION, 3002).
-define(CODE_AUTO_ADDITION, 3003).
-define(CODE_SUB_ACCOUNT_AUTO_ADDITION, 3004).
-define(CODE_ADMIN_DISCRETION, 3005).
-define(CODE_TOPUP, 3006).
-define(CODE_DATABASE_ROLLUP, 4000).
-define(CODE_RECURRING, 5000).
-define(CODE_MONTHLY_RECURRING, 5001).
-define(CODE_RECURRING_PRORATE, 5002).
-define(CODE_MOBILE, 6000).
-define(CODE_UNKNOWN, 9999).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ONBILL_HRL, 'true').
-endif.

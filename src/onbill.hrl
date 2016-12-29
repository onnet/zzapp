-ifndef(ONBILL_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-define(APP_NAME, <<"onbill">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(SERVICES_DB, <<"services">>).
-define(ONBILL_DB, <<"onbill">>).
-define(ONBILL_DOC, <<"onbill">>).
-define(MRC_DOC, <<"monthly_recurring">>).
-define(TO_BIN(Var), kz_util:to_binary(Var)).
-define(TO_INT(Var), kz_util:to_integer(Var)).
-define(TO_FLT(Var), kz_util:to_float(Var)).
-define(CARRIER_DOC(CarrierName), <<"onbill_carrier.", CarrierName/binary>>).
-define(MOD_CONFIG_CRAWLER, <<(?APP_NAME)/binary, ".account_crawler">>).
-define(DOCS_NUMBER_DB(ResellerId, Year), <<(?APP_NAME)/binary
                                           ,"-"
                                           ,ResellerId/binary
                                           ,"-"
                                           ,(kz_util:to_binary(Year))/binary>>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(DOC_NAME_FORMAT(Carrier, TemplateID), <<Carrier/binary, "_", TemplateId/binary>>).
-define(HTML_TO_PDF, <<"/usr/local/bin/wkhtmltopdf --quiet">>).
-define(DEFAULT_REGEX, <<"^\\d*$">>).
-define(START_DATE(Month, Year), <<"01.",(kz_util:pad_month(Month))/binary,".",(kz_util:to_binary(Year))/binary>>).
-define(END_DATE(Month, Year), <<(kz_util:to_binary(calendar:last_day_of_the_month(Year, Month)))/binary
                                ,"."
                                ,(kz_util:pad_month(Month))/binary
                                ,"."
                                ,(kz_util:to_binary(Year))/binary>>).
-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).

-define(ONBILL_HRL, 'true').
-endif.

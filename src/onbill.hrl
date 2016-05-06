-ifndef(ONBILL_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"onbill">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(ONBILL_ACCOUNTS_DB, <<"onbill_accounts">>).
-define(ONBILL_DOC, <<"onbill">>).
-define(MOD_CONFIG_TEMLATES(CarrierName), <<(?APP_NAME)/binary, ".", (wh_util:to_binary(CarrierName))/binary, ".templates">>).
-define(MOD_CONFIG_CRAWLER, <<(?APP_NAME)/binary, ".account_crawler">>).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(HTML_TO_PDF(TemplateId), <<"php applications/onbill/priv/templates/ru/", (wh_util:to_binary(TemplateId))/binary, ".php">>).

-define(ONBILL_HRL, 'true').
-endif.

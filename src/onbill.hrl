-ifndef(ONBILL_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"onbill">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(ONBILL_ACCOUNTS_DB, <<"onbill_accounts">>).
-define(MOD_CONFIG_TEMLATES, <<(?APP_NAME)/binary, ".templates">>).
-define(MOD_CONFIG_CRAWLER, <<(?APP_NAME)/binary, ".account_crawler">>).
-define(DEFAULT_TEMPLATE(TemplateId), <<"<p>Dear Mr. {{ name }},</p><p>Please replace it with "
                                        ,(wh_util:to_binary(TemplateId))/binary
                                        ," template.</p><br /><p>Best regards,</p>">>
       ).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(HTML_TO_PDF, <<"php /tmp/iamtcpdftest.php">>).

-define(ONBILL_HRL, 'true').
-endif.

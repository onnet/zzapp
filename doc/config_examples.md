
```
   "pvt_onbill_reseller_vars": {
       "carriers": [
           "onnet",
           "beeline"
       ],
       "vat_disposition": "brutto",
       "vat_rate": 18,
       "extra_codes": {
           "phone_numbers": {
               "code_number": 796,
               "code_name": "шт"
           },
           "number_activation": {
               "code_number": 796,
               "code_name": "шт"
           },
           "monthly_services": {
               "code_number": 796,
               "code_name": "шт"
           },
           "default": {
               "code_number": "-",
               "code_name": "-"
           }
       },
       "currency1": "руб"
   },
```

```
   "pvt_onbill_account_vars": {
       "language": "ru-RU",
       "billing_address": {
           "line1": "191186, Санкт-Петербург",
           "line2": "Б. Морская д. 3/5",
           "line3": "Офис 505"
       },
       "account_name": "ЗАО «Тест»",
       "account_inn": 1234567890,
       "account_kpp": "098765",
       "aggregate_invoice": true,
       "agrm": {
           "onnet": {
               "number": "1-test",
               "date": "01.01.2001"
           },
           "beeline": {
               "number": "0WG#1-test",
               "date": "01.01.2001"
           }
       }
   },
```

```
{
   "_id": "onbill_carrier.onnet",
   "_rev": "12-df32e768398fab9203012b553b920e98",
   "oper_name": "Закрытое Акционерное Общество «ОнНет комьюникейшнс»",
   "oper_addr": "194156, г. Санкт-Петербург, пр-кт Энгельса, д. 27 лит. З",
   "inn": 7802201145,
   "kpp": 780201001,
   "oper_buh": "Сысоева И.А.",
   "oper_dir": "Сысоев К.В.",
   "comment1": "Оплате подлежит Единый счет на расчетный счет ЗАО «ОнНет комьюникейшнс» Получатель: ЗАО «ОнНет комьюникейшнс» ПАО АКБ «ЮГРА» г.Санкт-Петербург Банковские реквизиты: p/c 40702810702110000036, к/с 30101810900000000774, БИК 044030774",
   "per_minute_item_name": "Местные вызовы СПб",
   "oper_name_short": "ЗАО «ОнНет комьюникейшнс»",
   "oper_signatory": "ОПЕРАТОР",
   "onbill_doc_types": [
       "invoice",
       "act"
   ],
   "oper_name_further": "«Оператор»",
   "carrier_type": "main",
   "oper_account_number": 40702810702110000000,
   "oper_bank_name": "Филиал ПАО Банк «ЮГРА» в г. Санкт-Петербурге, г. Санкт-Петербург",
   "oper_bank_bik": "044030774",
   "oper_corr_number": 30101810900000002000,
   "pvt_vsn": "1",
   "pvt_account_id": "b35483a7e18684f4cd5f09eae01a4c0c",
   "pvt_modified": 63633914755,
   "pvt_request_id": "368d5bfe70c8d807ac753079f00f7fe7",
   "pvt_auth_user_id": "8b81351bc3afaf4c34d67eb799438498",
   "pvt_auth_account_id": "b35483a7e18684f4cd5f09eae01a4c0c",
   "pvt_is_authenticated": true,
   "_attachments": {
       "onnet_calls_report.tpl": {
           "content_type": "text/html",
           "revpos": 12,
           "digest": "md5-MaWLyusQ6pizZeIEzIqXXQ==",
           "length": 3535,
           "stub": true
       },
       "onnet_act.tpl": {
           "content_type": "text/html",
           "revpos": 11,
           "digest": "md5-j6ilJ+L5/H9dpxVXGuiWgA==",
           "length": 3836,
           "stub": true
       },
       "onnet_invoice.tpl": {
           "content_type": "text/html",
           "revpos": 10,
           "digest": "md5-jp4oHvfxoBjvVvokFtxAuw==",
           "length": 7528,
           "stub": true
       }
   }
}
```
```
{
   "_id": "onbill_carrier.beeline",
   "_rev": "8-ae81f45f8bbf5d8b6511abd4b2e91801",
   "oper_name": "Публичное Акционерное Общество «Вымпел-Коммуникации»",
   "oper_addr": "127083, г. Москва, ул. Восьмого марта, д. 10 стр. 14",
   "inn": 7713076301,
   "kpp": 997750001,
   "oper_buh": "Сысоев К.В.",
   "oper_dir": "Сысоев К.В.",
   "comment1": "Оплате подлежит Единый счет на расчетный счет ЗАО «ОнНет комьюникейшнс» Получатель: ЗАО «ОнНет комьюникейшнс» ПАО АКБ «ЮГРА» г.Санкт-Петербург Банковские реквизиты: p/c 40702810702110000036, к/с 30101810900000000774, БИК 044030774",
   "per_minute_item_name": "Междугородная и международная связь",
   "oper_name_short": "ПАО «Вымпел-Коммуникации»",
   "oper_signatory": "ОПЕРАТОР",
   "caller_number_regex": "^7812\\d*$",
   "called_number_regex": "^(?!7812)\\d*$",
   "onbill_doc_types": [
       "invoice",
       "act"
   ],
   "oper_name_further": "«Оператор»",
   "pvt_vsn": "1",
   "pvt_account_id": "b35483a7e18684f4cd5f09eae01a4c0c",
   "pvt_modified": 63633201823,
   "pvt_request_id": "b0aa3dd6d099eb8f66e28b2ca942d36b",
   "pvt_auth_user_id": "8b81351bc3afaf4c34d67eb799438498",
   "pvt_auth_account_id": "b35483a7e18684f4cd5f09eae01a4c0c",
   "pvt_is_authenticated": true,
   "_attachments": {
       "beeline_calls_report.tpl": {
           "content_type": "text/html",
           "revpos": 8,
           "digest": "md5-MaWLyusQ6pizZeIEzIqXXQ==",
           "length": 3535,
           "stub": true
       },
       "beeline_act.tpl": {
           "content_type": "text/html",
           "revpos": 7,
           "digest": "md5-j6ilJ+L5/H9dpxVXGuiWgA==",
           "length": 3836,
           "stub": true
       },
       "beeline_invoice.tpl": {
           "content_type": "text/html",
           "revpos": 6,
           "digest": "md5-jp4oHvfxoBjvVvokFtxAuw==",
           "length": 7528,
           "stub": true
       }
   }
}

```

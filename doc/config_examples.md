
onbill doc reseller's example
```
    {  "_id": "onbill",
       "iso_code_country_of_residence": "ru",
       "default_service_plan": "voip_service_plan",
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
    }
```
onbill doc account's example
```
    {  "_id": "onbill",
       "iso_code_country_of_residence": "ru",
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
   }
```

```
{
   "_id": "onbill_carrier.onnet",
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
   "oper_corr_number": 30101810900000002000
}
```
```
{
   "_id": "onbill_carrier.beeline",
   "continious_doc_numbering": true,
   "doc_pref": "IO#",
   "doc_ind": "-BK",
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
   "caller_number_regex": "^\\+?7812\\d*$",
   "called_number_regex": "^\\+?(?!7812)\\d*$",
   "onbill_doc_types": [
       "invoice",
       "act"
   ],
   "oper_name_further": "«Оператор»"
}

```

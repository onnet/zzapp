<head>
  <meta charset="UTF-8">
  <link href='http://fonts.googleapis.com/css?family=PT+Sans+Narrow&subset=latin,cyrillic' rel='stylesheet' type='text/css'>
  <style>

    * {
      font-family: 'PT Sans Narrow';
      font-size: 13;
    }

    table {
        width: 100%;
    }

    .bordered_table {
      border-collapse: collapse;
    }

    .bordered_table tr,
    .bordered_table td {
      border: 1px solid black;
      text-align: center;
    }

  </style>
</head>
<body>
  <table border=0>
    <tr>
      <td width="80%" align="center">
          <font size=12>СЧЕТ-ФАКТУРА № {{ carrier_vars.doc_pref }}{{ doc_number }}{{ carrier_vars.doc_ind }} от {{ agrm_date }}
        <br />
        ИСПРАВЛЕНИЕ № <u> --- </u> от <u>   -------   </u></font>
        <br />
        за оказанные услуги электросвязи за период: {{ start_date }} - {{ end_date }}
      </td>
      <td align="right" width="20%">
        <span style="font-size: 7;">	
          Приложение №1<br />
          к постановлению Правительства<br />
          Российской Федерации<br />
          от 26.12.2011 г. № 1137
        </span>
      </td>
    </tr>
  </table>
  Продавец: {{ carrier_vars.oper_name }}<br />
  Адрес: {{ carrier_vars.oper_addr }}<br />
  ИНН/КПП продавца: {{ carrier_vars.inn }} / {{ carrier_vars.kpp }} <br />
  {% if carrier_vars.oper_agent %}
    Агент: {{ carrier_vars.oper_agent }} ({{ carrier_vars.oper_agent_short }}) <br />
    Адрес агента: {{ carrier_vars.oper_agent_addr }} <br />
    ИНН/КПП агента: {{ carrier_vars.oper_agent_inn }} / {{ carrier_vars.oper_agent_kpp }} <br />
  {% endif %}
  Грузоотправитель и его адрес: ----<br />
  Грузополучатель и его адрес: ----<br />
  К платежно-расчетному документу № ______ от ______ <br />
  Покупатель: {{ account_vars.account_name }}<br />
  Адрес:
    {{ account_vars.billing_address.line1 }}
    {{ account_vars.billing_address.line2 }}
    {{ account_vars.billing_address.line3 }}
    <br />
  ИНН/КПП покупателя: {{ account_vars.account_inn }} / {{ account_vars.account_kpp }} <br />
  Валюта: наименование, код <u>Российский рубль, 643</u><br />
  Идентификатор государственного контракта, договора (соглашения): ---- <br />
  <font size="9">
    Дополнительные (условия оплаты по договору (контракту), способ отправления и т.д.)
  </font>
  Договор № {{ carrier_vars.doc_pref }}{{ agrm_num }} от {{ agrm_date }}
  <br />
  <table class="bordered_table" style="width: 100%;">
    <tr>
      <td align="center" width="23%" rowspan="2">
        <font size="9">
  	Наименование товара<br />(описание выполненных работ, оказанных услуг), имущественного права
        </font>
      </td>
      <td align="center" width="11%" colspan=2>
        <font size="9">
          Единица<br />измерения
        </font>
      </td>
      <td align="center" width="5%" rowspan="2">
        <font size="9">
          Коли-<br />чество (объем)<br />
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="9">
          Цена (тариф) за единицу измерения<br />
        </font>
      </td>
      <td align="center" width="10%" rowspan="2">
        <font size="9">
          Стоимость товаров (работ, услуг), имущественных прав без налога — всего<br />
        </font>
      </td>
      <td align="center" width="5%" rowspan="2">
        <font size="9">
          В том числе сумма акциза<br />
        </font>
      </td>
      <td align="center" width="6%" rowspan="2">
        <font size="9">
          Налоговая ставка<br />
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="9">
          Сумма налога, предъявляе-<br />мая покупателю<br />
        </font>
      </td>
      <td align="center" width="10%" rowspan="2">
        <font size="9">
          Стоимость товаров (работ, услуг), имущественных прав с налогом — всего<br />
        </font>
      </td>
      <td align="center" width="12%" colspan=2>
        <font size="9">
          Страна происхождения товара<br />
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="9">
          Номер таможенной декларации<br />
        </font>
      </td>
    </tr>
    <tr>
      <td width="3%" align="center">
        <font size="8">
          к<br />о<br />д<br />
        </font>
      </td>
      <td width="8%" align="center">
        <font size="8">
          условное обозначение (национальное)<br />
        </font>
      </td>
      <td width="4%" align="center">
        <font size="8">
          цифро-<br />вой код<br />
        </font>
      </td>
      <td width="8%" align="center">
        <font size="8">
          краткое наименование<br />
        </font>
      </td>
    </tr>
    <tr>
      <td width="23%">
        <font size="8">1</font>
      </td>
      <td width="3%">
        <font size="8">2</font>
      </td>
      <td width="8%">
        <font size="8">2а</font>
      </td>
      <td width="5%">
        <font size="8">3</font>
      </td>
      <td width="7%">
        <font size="8">4</font>
      </td>
      <td width="10%">
        <font size="8">5</font>
      </td>
      <td width="5%">
        <font size="8">6</font>
      </td>
      <td width="6%">
        <font size="8">7</font>
      </td>
      <td width="7%">
        <font size="8">8</font>
      </td>
      <td width="10%">
        <font size="8">9</font>
      </td>
      <td width="4%">
        <font size="8">10</font>
      </td>
      <td width="8%">
        <font size="8">10а</font>
      </td>
      <td width="7%">
        <font size="8">11</font>
      </td>
    </tr>
    <!-- begin_services -->
    <!-- begin_item -->
    {% for fee_line in monthly_fees %}
      <tr>
        <td width="23%" style="text-align: left;">
          {{ fee_line.name }}
          {% if fee_line.period %}
            {% for period in fee_line.period %}
               {{ period.day }}
               {{ period.month_short }}
               {{ period.year }}{% if forloop.last %}.{% endif %}
            {% endfor %}
          {% endif %}
        </td>
        <td width="3%">{{ fee_line.code_number }}</td>
        <td width="8%">{{ fee_line.code_name }}</td>
        <td width="5%" style="text-align: right;">{{ fee_line.quantity|floatformat:2 }}</td>
        <td width="7%" style="text-align: right;">{{ fee_line.rate_netto|floatformat:2 }}</td>
        <td width="10%" style="text-align: right;">{{ fee_line.cost_netto|floatformat:2 }}</td>
        <td width="5%">Без акциза</td>
        <td width="6%" style="text-align: right;">{{ vat_rate }}%</td>
        <td width="7%" style="text-align: right;">{{ fee_line.vat_line_total|floatformat:2 }}</td>
        <td width="10%" style="text-align: right;">{{ fee_line.cost_brutto|floatformat:2 }}</td>
        <td width="4%">--</td>
        <td width="8%">--</td>
        <td width="7%">--</td>
      </tr>
    {% endfor %}
    <!-- end_item -->
    <!-- end_services -->
    <tr>
      <td width="46%" style="text-align: left;" colspan=5>Всего к оплате:</td>		
      <td width="10%" style="text-align: right;">{{ total_netto|floatformat:2 }}</td>
      <td width="11%" colspan="2">X</td>
      <td width="7%" style="text-align: right;">{{ total_vat|floatformat:2 }}</td>
      <td width="10%" style="text-align: right;">{{ total_brutto|floatformat:2 }}</td>
    </tr>	
  </table>
  <br />
  <table style="width: 100%;">
    <tr>
      <td width="50%">
        <table border="0">
          <tr>
            <td colspan=2>
              Руководитель организации
              <br />
              или иное уполномоченное лицо
              _________________________________________
              ({{ carrier_vars.oper_dir }})
            </td>
          </tr>
          <tr>
            <td  style="width: 35%;">
              <span style="padding-left: 23em; font-size: 10;">
                (подпись)
              </span>
            </td>
            <td>
              <span style="padding-left: 8em; font-size: 10;">
                (ф.и.о.)
              </span>
            </td>
          </tr>
          {% if carrier_vars.oper_power %}
            <tr>
              <td colspan=2>
                <span style="padding-left: 13em; font-size: 11;">
                  (на основании доверенности {{ carrier_vars.oper_power }})
                </span>
              </td>
            </tr>
          {% endif %}
        </table>			
      </td>
      <td width="50%">
        <table border="0">
          <tr>
            <td colspan=2>
              Главный бухгалтер<br />
              или иное уполномоченное лицо
              _________________________________________
              ({{ carrier_vars.oper_buh }})
            </td>
          </tr>
          <tr>
            <td  style="width: 35%;">
              <span style="padding-left: 23em; font-size: 10;">
                (подпись)
              </span>
            </td>
            <td>
              <span style="padding-left: 8em; font-size: 10;">
                (ф.и.о.)
              </span>
            </td>
          </tr>
          {% if carrier_vars.oper_power %}
            <tr>
              <td colspan=2>
                <span style="padding-left: 13em; font-size: 11;">
                  (на основании доверенности {{ carrier_vars.oper_power }})
                </span>
              </td>
            </tr>
          {% endif %}
        </table>	
      </td>
    </tr>
  </table>
  {% if carrier_vars.comment1 %}
    <br />
    {{ carrier_vars.comment1 }}
    <br />
    <br />
  {% endif %}
  <span style="font-size: 10;">
    Примечание. Первый экземпляр — покупателю, второй экземпляр — продавцу
  </span>
</body>

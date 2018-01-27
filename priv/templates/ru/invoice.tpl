{% load onbill_dtl %}
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

    tr { page-break-inside: avoid; }

    .bordered_table {
      border-collapse: collapse;
    }

    .bordered_table tr,
    .bordered_table td {
      border: 1px solid black;
      text-align: center;
    }

    .cell_padding {
      padding: 3px;
    }

  </style>
</head>
<body>
  <table border=0>
    <tr>
      <td width="80%" align="center">
          <font size=12>СЧЕТ-ФАКТУРА № {{ carrier_vars.doc_pref }}{{ doc_number }}{{ carrier_vars.doc_ind }} от {{ doc_date }}
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
          от 26.12.2011 г. № 1137<br />
          (в ред. Постановления Правительства РФ от 19.08.2017 № 981)
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
  Покупатель: {{ account_vars.name }}<br />
  Адрес:
    {{ account_vars.address.registered.line1 }}
    {{ account_vars.address.registered.line2 }}
    {{ account_vars.address.registered.line3 }}
    <br />
  ИНН/КПП покупателя: {{ account_vars.inn }} / {{ account_vars.kpp }} <br />
  Валюта: наименование, код <u>Российский рубль, 643</u><br />
  Идентификатор государственного контракта, договора (соглашения) (при наличии): ---- <br />
  <font size="9">
    Дополнительные (условия оплаты по договору (контракту), способ отправления и т.д.)
  </font>
  Договор № {{ agrm_num }} от {{ agrm_date|gregsec_to_date|date:"d.m.Y" }}
  <br />
  <table class="bordered_table" style="width: 100%;">
    <tr>
      <td align="center" width="24%" rowspan="2">
        <font size="8">
  	Наименование товара<br />(описание выполненных работ, оказанных услуг), имущественного права
        </font>
      </td>
      <td align="center" width="4%" rowspan="2">
        <font size="8">
                Код вида товара
        </font>
      </td>
      <td align="center" width="8%" colspan=2>
        <font size="8">
          Единица<br />измерения
        </font>
      </td>
      <td align="center" width="5%" rowspan="2">
        <font size="8">
          Коли-<br />чество (объем)
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="8">
          Цена (тариф) за единицу измерения
        </font>
      </td>
      <td align="center" width="9%" rowspan="2">
        <font size="8">
          Стоимость товаров (работ, услуг), имущественных прав без налога — всего
        </font>
      </td>
      <td align="center" width="5%" rowspan="2">
        <font size="8">
          В том числе сумма акциза
        </font>
      </td>
      <td align="center" width="6%" rowspan="2">
        <font size="8">
          Налоговая ставка
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="8">
          Сумма налога, предъявляе-<br />мая покупателю
        </font>
      </td>
      <td align="center" width="9%" rowspan="2">
        <font size="8">
          Стоимость товаров (работ, услуг), имущественных прав с налогом — всего
        </font>
      </td>
      <td align="center" width="9%" colspan=2>
        <font size="8">
          Страна происхождения товара
        </font>
      </td>
      <td align="center" width="7%" rowspan="2">
        <font size="8">
          Регистрацион-<br />ный номер таможенной декларации
        </font>
      </td>
    </tr>
    <tr>
      <td width="2%" align="center">
        <font size="6">
          к<br />о<br />д<br />
        </font>
      </td>
      <td width="6%" align="center">
        <font size="6">
          условное обозначение (национальное)<br />
        </font>
      </td>
      <td width="3%" align="center">
        <font size="6">
          цифро-<br />вой код<br />
        </font>
      </td>
      <td width="6%" align="center">
        <font size="6">
          краткое наименование<br />
        </font>
      </td>
    </tr>
    <tr>
      <td width="24%">
        <font size="7">1</font>
      </td>
      <td width="4%">
        <font size="7">1a</font>
      </td>
      <td width="2%">
        <font size="7">2</font>
      </td>
      <td width="6%">
        <font size="7">2а</font>
      </td>
      <td width="5%">
        <font size="7">3</font>
      </td>
      <td width="7%">
        <font size="7">4</font>
      </td>
      <td width="9%">
        <font size="7">5</font>
      </td>
      <td width="5%">
        <font size="7">6</font>
      </td>
      <td width="6%">
        <font size="7">7</font>
      </td>
      <td width="7%">
        <font size="7">8</font>
      </td>
      <td width="9%">
        <font size="7">9</font>
      </td>
      <td width="3%">
        <font size="7">10</font>
      </td>
      <td width="6%">
        <font size="7">10а</font>
      </td>
      <td width="7%">
        <font size="7">11</font>
      </td>
    </tr>
    <!-- begin_services -->
    <!-- begin_item -->
    {% for fee_line in monthly_fees %}
      {% if fee_line.cost|floatformat:2 != "0.00" %}
        <tr>
          <td class="cell_padding" style="text-align: left; width: 24%;">
            {{ fee_line.name }}
            {% if fee_line.period and fee_line.days_quantity != fee_line.days_in_period %}
              {% for period in fee_line.period %}
                 {{ period.day }}
                 {% if period.month_short == "Jan" %}
                   янв.
                 {% elif period.month_short == "Feb" %}
                   февр.
                 {% elif period.month_short == "Mar" %}
                   март
                 {% elif period.month_short == "Apr" %}
                   апр.
                 {% elif period.month_short == "May" %}
                   май
                 {% elif period.month_short == "Jun" %}
                   июнь
                 {% elif period.month_short == "Jul" %}
                   июль
                 {% elif period.month_short == "Aug" %}
                   авг.
                 {% elif period.month_short == "Sep" %}
                   сент.
                 {% elif period.month_short == "Oct" %}
                   откт.
                 {% elif period.month_short == "Nov" %}
                   нояб.
                 {% elif period.month_short == "Dec" %}
                   дек.
                 {% else %}
                   {{ period.month_pad }}
                 {% endif %}
                 {{ period.year }}{% if forloop.last %}.{% endif %}
              {% endfor %}
            {% endif %}
          </td>
          <td class="cell_padding" style="width: 4%;">--</td>
          <td class="cell_padding" style="width: 2%;">{{ fee_line.code_number }}</td>
          <td class="cell_padding" style="width: 6%;">{{ fee_line.code_name }}</td>
          <td class="cell_padding" style="text-align: right; width: 5%;">{{ fee_line.quantity|floatformat:2 }}</td>
          <td class="cell_padding" style="text-align: right; width: 7%;">{{ fee_line.rate_netto|floatformat:2 }}</td>
          <td class="cell_padding" style="text-align: right; width: 9%;">{{ fee_line.cost_netto|floatformat:2 }}</td>
          <td style="width: 5%;">Без акциза</td>
          <td class="cell_padding" style="text-align: right; width: 6%;">{{ vat_rate }}%</td>
          <td class="cell_padding" style="text-align: right; width: 7%;">{{ fee_line.vat_line_total|floatformat:2 }}</td>
          <td class="cell_padding" style="text-align: right; width: 9%;">{{ fee_line.cost_brutto|floatformat:2 }}</td>
          <td style="width: 3%;">--</td>
          <td style="width: 6%;">--</td>
          <td style="width: 7%;">--</td>
        </tr>
      {% endif %}
    {% endfor %}
    <!-- end_item -->
    <!-- end_services -->
    <tr>
      <td class="cell_padding" style="text-align: left; width: 48%;" colspan=5>Всего к оплате:</td>		
      <td class="cell_padding" style="text-align: right; width: 9%;">{{ total_netto|floatformat:2 }}</td>
      <td colspan="2" style="width: 11%;">X</td>
      <td class="cell_padding" style="text-align: right; width: 7%;">{{ total_vat|floatformat:2 }}</td>
      <td class="cell_padding" style="text-align: right; width: 9%;">{{ total_brutto|floatformat:2 }}</td>
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
    <tr>
      <td width="100%" colspan="2">
        <font size=8>
          Индивидуальный предприниматель или иное уполномоченное лицо _______________________ (__________________________) 
          ____________________________________________________________________________________________________________________
        </font>
        <br />
        <table border="0">
          <tr>
            <td style="width: 18%; text-align: right; font-size: 10;"> </td> 
            <td style="width: 20%; text-align: right; font-size: 10;">(подпись)</td>
            <td style="width: 20%; text-align: center; font-size: 10;">(ф.и.о.)</td>
            <td style="width: 42%; text-align: center; font-size: 10;">
                (реквизиты свидетельства о государственной регистрации индивидуального предпринимателя)
            </td>
          </tr>
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

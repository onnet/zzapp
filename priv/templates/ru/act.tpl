<head>
  <meta charset="UTF-8">
  <link href='http://fonts.googleapis.com/css?family=PT+Sans+Narrow&subset=latin,cyrillic' rel='stylesheet' type='text/css'>
  <style>
    * {
      font-family: 'PT Sans Narrow';
      font-size: 15;
    }
  </style>
</head>
<body>
  <p style="text-align: center; font-size: 2em;">
    АКТ оказания услуг № {{ carrier_vars.doc_pref }}{{ doc_number }}{{ carrier_vars.doc_ind }} от {{ doc_date }}
  </p>
  <h4 align=center>
    по Договору №
    {{ agrm_num }} от {{ agrm_date }}
    <br />
    за оказанные услуги электросвязи за период: {{ start_date }} - {{ end_date }}
  </h4>
  <table width="100%">
    <tr>
      <td width="50%" align="left">г. Санкт-Петербург</td>
      <td width="49%" style="text-align: right;">
        {{ doc_date }}
      </td>
    </tr>
  </table>
  <h4 align=justify>
    {% if carrier_vars.oper_agent %}
      {{ carrier_vars.oper_name }} (далее — «{{ carrier_vars.oper_name_further }}»), организованное по законам
      Российской Федерации и имеющее местонахождение {{ carrier_vars.oper_addr }} в лице генерального директора
      {{ carrier_vars.oper_agent_short }} {{ carrier_vars.in_person_of }}, действующего по доверенности
      {{carrier_vars.oper_power }}, с одной стороны,
      и {{ account_vars.account_name }}, (далее — «Клиент»), с другой (далее — «Стороны»), составили настоящий Акт в том,
      что в соответствии с Договором об оказании услуг связи № {{ agrm_num }} от {{ agrm_date }},
      заключенным между Сторонами (далее — «Договор»), {{ carrier_vars.oper_name_further }} в указанный период оказал Клиенту Услуги:
    {% else %}
      {{ carrier_vars.oper_name }} (далее — «{{ carrier_vars.oper_name_further }}»), с одной стороны,
      и {{ account_vars.account_name }}, (далее — «Клиент»), с другой (далее — «Стороны»), составили настоящий Акт в том,
      что в соответствии с Договором об оказании услуг связи № {{ agrm_num }} от {{ agrm_date }},
      заключенным между Сторонами (далее — «Договор»),
      {{ carrier_vars.oper_name_further }} в указанный период оказал Клиенту Услуги:
    {% endif %}
  </h4>
  <table style="border-collapse: collapse;" width="100%">
    <tr style="border: 1px solid black;">
      <td style="border: 1px solid black;" width="5%"><h4 align="center">№</h4></td>
      <td style="border: 1px solid black;" width=44%><h4 align="center">Наименование услуги</h4></td>
      <td style="border: 1px solid black;" width=8%><h4 align="center">Ед. Изм.</h4></td>
      <td style="border: 1px solid black;" width=14%><h4 align="center">Кол-во</h4></td>
      <td style="border: 1px solid black;" width=14%><h4 align="center">Цена, {{ reseller_vars.currency_short }}.</h4></td>
      <td style="border: 1px solid black;" width=14%><h4 align="center">Сумма, {{ reseller_vars.currency1 }}.</h4></td>
    </tr>
  {% for fee_line in monthly_fees %}
    <tr style="text-align: center;">
      <td style="border: 1px solid black;">{{ forloop.counter }}</td>
      <td style="border: 1px solid black; text-align: left;">
        &nbsp
        {{ fee_line.name }}
        {% if fee_line.period %}
          {% for period in fee_line.period %}
             {{ period.day }}
             {{ period.month_short }}
             {{ period.year }}{% if forloop.last %}.{% endif %}
          {% endfor %}
        {% endif %}
      </td>
      <td style="border: 1px solid black;">{{ fee_line.code_name }}</td>
      <td style="border: 1px solid black;">{{ fee_line.quantity|floatformat:2 }}</td>
      <td style="border: 1px solid black; text-align: right;">{{ fee_line.rate_brutto|floatformat:2 }}</td>
      <td style="border: 1px solid black; text-align: right;">{{ fee_line.cost_brutto|floatformat:2 }}</td>
    </tr>
  {% endfor %}
    <tr>
      <td colspan="5" style="text-align: right; width: 85%;">Итого:</td>
      <td style="text-align: right; width: 14%;">{{ total_brutto|floatformat:2 }}</td>
    </tr>
    <tr>
      <td colspan="5" style="text-align: right; width: 85%;">
        В том числе НДС({{ vat_rate }}%):
      </td>
      <td style="text-align: right; width: 14%;">
        {{ total_vat|floatformat:2 }}
      </td>
    </tr>
  </table>
  <br />
  <span>
    Всего оказано услуг на сумму:
    {% if total_brutto_div %}
      {{ total_brutto_div }}
    {% else %}
      00
    {% endif %} руб.
    {% if total_brutto_rem %}
      {{ total_brutto_rem }}
    {% else %}
      00
    {% endif %}
    коп.,
    в т.ч. НДС({{ vat_rate }}%) —
    {% if total_vat_div %}
      {{ total_vat_div }}
    {% else %}
      00
    {% endif %}
    руб.
    {% if total_vat_rem %}
      {{ total_vat_rem }}
    {% else %}
      00
    {% endif %}
    коп.
  </span>
  <br />
  <br />
  <h4 align="center">
    Указанные услуги полностью соответствуют условиям Договора.
    <br />
    Клиент претензий к объему оказанных услуг и их качеству не имеет.
  </h4>
  <br />
  <br />
  <table width="100%">
    <tr>
      <td width="50%">
        <h4 align="center">
          <p>КЛИЕНТ</p>
         </h4>
      </td>
      <td>
        <h4 align="center">
          <p>{{ carrier_vars.oper_signatory }}<p>
        </h4>
      </td>
    </tr>
    <tr>
      <td>
        <h4 align="center">
          <p>________________________<p>
        </h4>
      </td>
      <td>
        <h4 align="center">
          <p>________________________</p>
        </h4>
      </td>
    </tr>
    <tr>
      <td>
        <h4 align="center">
          <p></p>
          <p>МП</p>
        </h4>
      </td>
      <td>
        <h4 align="center">
          ({{ carrier_vars.oper_dir }})
          <p>МП</p>
        </h4>
        {% if carrier_vars.oper_agent %}
          <p align="center">
            Агент {{ carrier_vars.oper_agent_short }}
            <br />
            (на основании доверенности {{ carrier_vars.oper_power }})
          </p>
        {% endif %}
      </td>
    </tr>
  </table>
</body>

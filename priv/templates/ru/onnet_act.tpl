<head>
  <meta charset="UTF-8">
</head>

<br />
<h1 align=center>АКТ оказания услуг </h1>
<h4 align=center>по Договору № {{ doc_pref }}{{ doc_number }}-{{ doc_ind }} от {{ doc_date }}<br />
за оказанные услуги электросвязи за период: {{ start_date }} - {{ end_date }}</h4>
<br />
<table width="100%">
  <tr>
    <td width="50%" align="left">г. Санкт-Петербург</td>
    <td width="49%" style="text-align: right;">
      {{ doc_date }}
    </td>
  </tr>
</table>
<br />
<h4 align=justify>{{ oper_name }} (далее — {{ oper_name_further }}), с одной стороны, и {{ account_name }}, (далее — «Клиент»), с другой (далее — «Стороны»), составили настоящий Акт в том, что в соответствии с Договором об оказании услуг связи № {{ agrm_num }} от {{ agrm_date }}, заключенным между Сторонами (далее — «Договор»), {{ oper_name_further }} в указанный период оказал Клиенту Услуги:
</h4><br />
<table style="border-collapse: collapse;">
  <tr style="border: 1px solid black;">
    <td style="border: 1px solid black;" width="5%"><h4 align="center">№</h4></td>
    <td style="border: 1px solid black;" width=44%><h4 align="center">Наименование услуги</h4></td>
    <td style="border: 1px solid black;" width=8%><h4 align="center">Ед. Изм.</h4></td>
    <td style="border: 1px solid black;" width=14%><h4 align="center">Кол-во</h4></td>
    <td style="border: 1px solid black;" width=14%><h4 align="center">Цена {{ currency1 }}.</h4></td>
    <td style="border: 1px solid black;" width=14%><h4 align="center">Сумма {{ currency1 }}.</h4></td>
  </tr>
{% for fee_line in monthly_fees %}
  <tr style="text-align: center;">
    <td style="border: 1px solid black;">{{ forloop.counter }}</td>
    <td style="border: 1px solid black;">
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
    <td style="border: 1px solid black;">{{ fee_line.quantity }}</td>
    <td style="border: 1px solid black;">{{ fee_line.rate_brutto|floatformat:2 }}</td>
    <td style="border: 1px solid black;">{{ fee_line.cost_brutto|floatformat:2 }}</td>
  </tr>
{% endfor %}
  <tr>
    <td colspan="5" align="right" width="85%">Итого:</td>
    <td width="14%"  style="text-align: center;">{{ total_brutto|floatformat:2 }}</td>
  </tr>
  <tr>
    <td colspan="5" align="right" width="85%">В том числе НДС({{ vat_rate }}%):</td>
    <td width="14%"  style="text-align: center;">{{ total_vat|floatformat:2 }}</td>
  </tr>
 </table>
<br />Всего оказано услуг на сумму:
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
  00{% endif %}
коп.
<br />
<br />
<h4 align="center">Указанные услуги полностью соответствуют условиям Договора. <br />
Клиент претензий к объему оказанных услуг и их качеству не имеет.</h4><br /><br />
<table>
  <tr>
    <td width=50%>
      <h4 align="center">КЛИЕНТ<br /><br /><br />________________________<br /><br /><br />МП</h4>
    </td>
    <td width=49%>
      <h4 align="center">{{ oper_signatory }}<br /><br /><br />________________________<br />({{ oper_dir }})<br /><br />МП</h4><p align="center">{{ oper_agent }}<br />{{ oper_power }}</p>
    </td>
  </tr>
</table>

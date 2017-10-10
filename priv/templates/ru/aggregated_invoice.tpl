<head>
  <meta charset="UTF-8">
  <link href='http://fonts.googleapis.com/css?family=PT+Sans+Narrow&subset=latin,cyrillic' rel='stylesheet' type='text/css'>
  <style>

    * {
      font-family: 'PT Sans Narrow';
      font-size: 15;
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
      padding: 3px;
    }

  </style>
</head>

<br />
<table border="0">
  <tr>
    <td width="60%">
      Поставщик: {{ carrier_vars.oper_name_short }}<br />
      Адрес: {{ carrier_vars.oper_addr }}<br />
      ИНН/КПП: {{ carrier_vars.inn }} / {{ carrier_vars.kpp }}<br />
    </td>
    <td width="40%">
      Счет №: {{ carrier_vars.oper_account_number }}<br />
      Банк получателя: {{ carrier_vars.oper_bank_name }}<br />
      БИК: {{ carrier_vars.oper_bank_bik }}<br />
      Кор. Счет №: {{ carrier_vars.oper_corr_number }}
    </td>
  </tr>
</table>

<br />
<br />
<h1 align="center">
  Счет № {{ doc_number }} от {{ end_date }}
</h1>
<h4 align="center">
  за оказанные услуги электросвязи за период: {{ start_date }} — {{ end_date }}
</h4>
<br />
Плательщик: {{ account_vars.account_name }}<br />
ИНН/КПП: {{ account_vars.account_inn }} / {{ account_vars.account_kpp }}<br />
<br />

<table class="bordered_table" style="width: 100%;">
  <tr>
    <td style="text-align: center; width: 5%">№</td>
    <td style="text-align: center; width: 80%">Наименование товара</td>
    <td style="text-align: center; width: 15%">Сумма (руб)</td>
  </tr>
  {% for carrier_line in aggregated_vars %}
    <tr>
      <td style="text-align: center; width: 5%">
        {{ forloop.counter }}
      </td>
      <td style="text-align: left; width: 80%">
        Услуги {{ carrier_line.oper_name_short }}.
        Договор № {{ carrier_line.agrm_num }} от {{ carrier_line.agrm_date }}
      </td>
      <td style="text-align: right; width: 15%">
        {{ carrier_line.total_brutto }}
      </td>
    </tr>
  {% endfor %}
  <tr style="border: 0!important;">
    <td colspan="2" style="text-align: right; width: 85%; border: 0!important;">
      Всего к оплате:
    </td>
    <td style="text-align: right; width: 15%">
      {{ total_brutto }}
    </td>		
  </tr>
  <tr style="border: 0!important;">
    <td colspan="2"
        style="text-align: right; width: 85%; pading-right: 3px; border: 0!important;">
      В том числе НДС ({{ vat_rate }}%)
    </td>
    <td style="text-align: right; width: 15%; padding-right: 3px;">
      {{ total_vat }}
    </td>		
  </tr>
</table> 
<br />
Сумма прописью:
{% if total_brutto_div %}{{ total_brutto_div }}{% else %}00{% endif %} руб.
{% if total_brutto_rem %}{{ total_brutto_rem }}{% else %}00{% endif %} коп.
<br />
Счет действителен в течение 14 календарных дней
<br />
<br />
<br />
<h4>Генеральный директор______________________________________________{{ carrier_vars.oper_dir }}</h4>
<br />
<br />
<br />
<h4>Главный бухгалтер_________________________________________________{{ carrier_vars.oper_buh }}</h4>
<br />
<h4 align="center">МП</h4>

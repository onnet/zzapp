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
      page-break-after:avoid;
    }

    .cell_padding {
      padding: 3px;
    }

  </style>
</head>

<table style="width: 197mm;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
  <tr>
    <td>Поставщик:</td>
    <td>{{ carrier_vars.oper_name_short }} ИНН: {{ carrier_vars.inn }} КПП: {{ carrier_vars.kpp }}</td>
  </tr>
  <tr>
    <td>Адрес:</td>
    <td>{{ carrier_vars.oper_addr }}</td>
  </tr>
</table>
<br />
<div style="text-align: center;"><strong>Детализация телефонных вызовов {{ account_name }}</strong></div>
<div style="text-align: center; font-size: 9pt;">Договор № {{ agrm_num }} от {{ agrm_date }}. Период с {{ start_date }} по {{ end_date }}.</div><br />
<table class="bordered_table">
  <tr>
    <td style="width: 4%; font-size: 9pt;">№</td>
    <td style="width: 9%; font-size: 9pt;">Номер А</td>
    <td style="width: 9%; font-size: 9pt;">Номер Б</td>
    <td style="width: 22%; font-size: 9pt;">Начало звонка</td>
    <td style="width: 8%; font-size: 9pt;">Длительность, мин.</td>
    <td style="width: 43%; font-size: 9pt;">Направление</td>
    <td style="width: 5%; font-size: 9pt;">Сумма, руб.</td>
  </tr>
  {% for per_minute_call in per_minute_calls %}
    <tr>
      <td style="width: 4%; font-size: 9pt;">{{ forloop.counter }}</td>
      <td style="width: 9%; font-size: 9pt;">{{ per_minute_call.value.from }}</td>
      <td style="width: 9%; font-size: 9pt;">{{ per_minute_call.value.to }}</td>
      <td style="width: 22%; font-size: 9pt;">{{ per_minute_call.value.start_datetime }}</td>
      <td style="width: 8%; font-size: 9pt;">{{ per_minute_call.value.duration_min|floatformat:0 }}</td>
      <td style="width: 43%; font-size: 9pt;">{{ per_minute_call.value.rate_description }}</td>
      <td class="cell_padding" style="width: 5%; font-size: 9pt; text-align: right">{{ per_minute_call.value.cost|floatformat:2 }}</td>
    </tr>
  {% endfor %}
</table>
<table BORDER="0" CELLPADDING="0" CELLSPACING="0">
  <tr>
    <td style="width: 85%; text-align: right;">Итого:</td>
    <td style="width: 15%; text-align: right;">{{ total_brutto }} руб.</td>
  </tr>
  <tr>
    <td style="width: 85%; text-align: right;">В том числе НДС (18%):</td>
    <td style="width: 15%; text-align: right;">{{ total_vat }} руб.</td>
  </tr>
</table>
<br />
<br />
<table BORDER="0" CELLPADDING="0" CELLSPACING="0">
  <tr>
    <td style="width: 50%; text-align: left;">Генеральный директор</td>
    <td style="width: 50%; text-align: left;"></td>
  </tr>
  <tr>
    <td style="width: 50%; text-align: left;">ЗАО «ОнНет комьюникейшнс»</td>
    <td style="width: 50%; text-align: center;">Сысоев К.В.</td>
  </tr>
  <tr>
    <td style="width: 50%; text-align: right;">м.п.</td>
    <td style="width: 50%; text-align: left;"></td>
  </tr>
</table>

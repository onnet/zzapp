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

<TABLE style="width: 197mm;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<TR>
<TD>Поставщик:</TD>
<TD>{{ carrier_vars.oper_agent_short }} ИНН: {{ carrier_vars.inn }} КПП: {{ carrier_vars.kpp }}</TD>
</TR>
<TR>
<TD>Адрес:</TD>
<TD>{{ carrier_vars.oper_addr }}</TD>
</TR>
</TABLE>
<br />
<div style="text-align: center;"><strong>Детализация телефонных вызовов {{ account_name }}</strong></div>
<div style="text-align: center; font-size: 7pt;">Договор № {{ agrm_num }} от {{ agrm_date }}. Период с {{ start_date }} по {{ end_date }}.</div><br />
<TABLE style="width: 100%;" BORDER="1" CELLPADDING="0" CELLSPACING="0">
<TR>
<TD style="width: 4%; font-size: 9pt; vertical-align: bottom; text-align: center">№</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">Номер А</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">Номер Б</TD>
<TD style="width: 22%; font-size: 9pt; vertical-align: bottom; text-align: center">Начало звонка</TD>
<TD style="width: 8%; font-size: 9pt; vertical-align: bottom; text-align: center">Длительность, мин.</TD>
<TD style="width: 43%; font-size: 9pt; vertical-align: bottom; text-align: center">Направление</TD>
<TD style="width: 5%; font-size: 9pt; vertical-align: bottom; text-align: center">Сумма, руб.</TD>
</TR>
<!-- begin_row -->
{% for per_minute_call in per_minute_calls %}
<TR>
<TD style="width: 4%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ forloop.counter }}</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.from }}</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.to }}</TD>
<TD style="width: 22%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.start_datetime }}</TD>
<TD style="width: 8%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.duration }}</TD>
<TD style="width: 43%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.rate_description }}</TD>
<TD style="width: 5%; font-size: 9pt; vertical-align: bottom; text-align: center">{{ per_minute_call.value.cost|floatformat:2 }}</TD>
</TR>
{% endfor %}
<!-- end_row -->			
</TABLE>
<!-- end_have_stat -->		
<!-- end_service -->	
<TABLE style="width: 100%;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<tr>
<td style="width: 85%; text-align: right;">Итого:<td style="width: 15%; text-align: right;">{{ total_brutto }} руб.</td>
</tr>
<tr>
<td style="width: 85%; text-align: right;">В том числе НДС (18%):</td><td style="width: 15%; text-align: right;">{{ total_vat }} руб.</td>
</tr>
</TABLE>
<TABLE style="width: 100%;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<tr>
<td style="width: 50%; text-align: right;"></td><td style="width: 50%; text-align: right;"></td>
</tr>
<tr>
<td style="width: 50%; text-align: left;">Генеральный директор</td><td style="width: 50%; text-align: left;"></td>
</tr>
<tr>
<td style="width: 50%; text-align: left;">ЗАО «ОнНет комьюникейшнс»</td><td style="width: 50%; text-align: center;">Сысоев К.В.</td>
</tr>
<tr>
<td style="width: 50%; text-align: right;">м.п.</td><td style="width: 50%; text-align: left;"></td>
</tr>
</TABLE>

<head>
  <meta charset="UTF-8">
</head>

<br />
<table border="0"><tr>
	<td width="60%">
		Поставщик: {{ oper_name }}<br />
		Адрес: {{ oper_addr }}<br />
		ИНН/КПП: {{ inn }} / {{ kpp }}<br />
		
	</td>
	<td width="40%">
		Счет №: {{ oper_account_number }}<br />
		Банк получателя: {{ oper_bank_name }}<br />
		БИК: {{ oper_bank_bik }}<br />
		Кор. Счет №: {{ oper_corr_number }}
	</td></tr></table>

<h1 align="center">Счет № %ordernum% от {{ end_date }}</h1>
<h4 align="center">за оказанные услуги электросвязи за период: {{ start_date }} — {{ end_date }}</h4><br />
Плательщик: {{ account_name }}<br />
ИНН/КПП: {{ account_inn }} / {{ account_kpp }}<br />
<br />

<table border="1" cellpadding="2">
	<tr>
		<td width="5%">№</td>
		<td width="80%">Наименование товара</td>
		<td width="15%">Сумма (руб)</td>
	</tr>
<!-- begin_services -->	
{% for carrier_line in aggregated_vars %}
	<tr>
		<td width="5%">{{ forloop.counter }}</td>
		<td width="80%">Услуги {{ carrier_line.oper_name_short }}. Договор № {{ carrier_line.agrm_num }} от {{ carrier_line.agrm_date }}</td>
		<td width="15%">{{ carrier_line.total_brutto }}</td>
	</tr>
{% endfor %}
<!-- end_services -->	
	<tr>
		<td colspan="4" align="right" width="85%">Всего к оплате:</td>
		<td width="15%">{{ total_brutto }}</td>		
	</tr>
	<tr>
		<td colspan="4" align="right" width="85%">В том числе НДС ({{ vat_rate }})</td>
		<td width="15%">{{ total_vat }}</td>		
	</tr>
</table> 
<br />
Сумма прописью: %totalstr%
<br />
Счет действителен в течение 14 календарных дней<br /><br />
<h4>Генеральный директор____________________________________{{ oper_dir }}</h4>
<br /><h5 align="center"></h5>
<br /><br />
<h4>Главный бухгалтер_______________________________________{{ oper_buh }}</h4>
<br /><h5 align="center"></h5>
<h4 align="center">МП</h4>

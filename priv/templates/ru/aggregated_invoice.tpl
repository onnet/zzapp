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
		Счет №: %operrs%<br />
		Банк получателя: %operbank%<br />
		БИК: %operbik%<br />
		Кор. Счет №: %operks%
	</td></tr></table>

<h1 align="center">Счет № %ordernum% от %orderdate%</h1>
<h4 align="center">за оказанные услуги электросвязи за период: %start_date% — %end_date%</h4><br />
Плательщик: {{ account_name }}<br />
ИНН/КПП: {{ account_inn }} / {{ account_kpp }}<br />
<br />

<table border="1" cellpadding="2">
	<tr>
		<td width="5%">№</td>
		<td width="80%">Наименование товара</td>
		<td width="15%">Сумма (%curr%)</td>
	</tr>
<!-- begin_services -->	
	<tr>
		<td width="5%">%N%</td>
		<td width="80%">Услуги %item%. Договор № %dogovor% от %dogdate%</td>
		<td width="15%">%amount%</td>
	</tr>
<!-- end_services -->	
	<tr>
		<td colspan="4" align="right" width="85%">Всего к оплате:</td>
		<td width="15%">%total%</td>		
	</tr>
	<tr>
		<td colspan="4" align="right" width="85%">В том числе НДС (%ndsper%%)</td>
		<td width="15%">%nds%</td>		
	</tr>
</table> 
<br />
Сумма прописью: %totalstr%
<br />
Счет действителен в течение 14 календарных дней<br /><br />
<h4>Генеральный директор____________________________________%operdir%</h4>
<br /><h5 align="center"></h5>
<br /><br />
<h4>Главный бухгалтер_______________________________________%operbuh%</h4>
<br /><h5 align="center"></h5>
<h4 align="center">МП</h4>

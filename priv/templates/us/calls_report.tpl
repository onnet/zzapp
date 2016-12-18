<head>
  <meta charset="UTF-8">
</head>

<TABLE style="width: 197mm;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<TR>
<TD>Suplier:</TD>
<TD>{{ oper_name_short }}</TD>
</TR>
<TR>
<TD>Address:</TD>
<TD>{{ oper_addr }}</TD>
</TR>
</TABLE>
<br />
<div style="text-align: center;"><strong>Phone calls report for {{ account_name }}</strong></div>
<div style="text-align: center; font-size: 7pt;">Agreement # {{ agrm_num }} by {{ agrm_date }}. From {{ start_date }} to {{ end_date }}.</div><br />
<TABLE style="width: 100%;" BORDER="1" CELLPADDING="0" CELLSPACING="0">
<TR>
<TD style="width: 4%; font-size: 9pt; vertical-align: bottom; text-align: center">#</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">From</TD>
<TD style="width: 9%; font-size: 9pt; vertical-align: bottom; text-align: center">To</TD>
<TD style="width: 22%; font-size: 9pt; vertical-align: bottom; text-align: center">Start</TD>
<TD style="width: 8%; font-size: 9pt; vertical-align: bottom; text-align: center">Duration</TD>
<TD style="width: 43%; font-size: 9pt; vertical-align: bottom; text-align: center">Direction</TD>
<TD style="width: 5%; font-size: 9pt; vertical-align: bottom; text-align: center">Amount</TD>
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
<TD style="width: 5%; font-size: 9pt; vertical-align: bottom; text-align: center">${{ per_minute_call.value.cost|floatformat:2 }}</TD>
</TR>
{% endfor %}
<!-- end_row -->			
</TABLE>
<!-- end_have_stat -->		
<!-- end_service -->	
<TABLE style="width: 100%;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<tr>
<td style="width: 85%; text-align: right;">Total:<td style="width: 15%; text-align: right;">${{ total_brutto }}</td>
</tr>
</TABLE>
<TABLE style="width: 100%;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<tr>
<td style="width: 50%; text-align: right;"></td><td style="width: 50%; text-align: right;"></td>
</tr>
<tr>
<td style="width: 50%; text-align: left;">Linkcomtech Dev</td><td style="width: 50%; text-align: center;">Evgeny Yampolsky</td>
</tr>
</TABLE>

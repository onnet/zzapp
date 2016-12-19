<head>
  <meta charset="UTF-8">
  <style type="text/css">
                body { font-family: 'DejaVu Sans Condensed'; font-size: 11pt;  }
                p {     text-align: justify; margin-bottom: 4pt; margin-top:0pt;  }

                table {font-family: 'DejaVu Sans Condensed'; font-size: 9pt; line-height: 1.2;
                        margin-top: 2pt; margin-bottom: 5pt;
                        border-collapse: collapse; }

                thead { font-weight: bold; vertical-align: bottom; }
                tfoot { font-weight: bold; vertical-align: top; }
                thead td { font-weight: bold; }
                tfoot td { font-weight: bold; }

                .headerrow td, .headerrow th { background-gradient: linear #b7cebd #f5f8f5 0 1 0 0.2;  }
                .footerrow td, .footerrow th { background-gradient: linear #b7cebd #f5f8f5 0 1 0 0.2;  }

                th {    font-weight: bold;
                        vertical-align: top;
                        padding-left: 2mm;
                        padding-right: 2mm;
                        padding-top: 0.5mm;
                        padding-bottom: 0.5mm;
                 }

                td {    padding-left: 2mm;
                        vertical-align: top;
                        padding-right: 2mm;
                        padding-top: 0.5mm;
                        padding-bottom: 0.5mm;
                 }

                th p { margin:0pt;  }
                td p { margin:0pt;  }

                table.widecells td {
                        padding-left: 5mm;
                        padding-right: 5mm;
                }
                table.tallcells td {
                        padding-top: 3mm;
                        padding-bottom: 3mm;
                }

                hr {    width: 70%; height: 1px;
                        text-align: center; color: #999999;
                        margin-top: 8pt; margin-bottom: 8pt; }

                a {     color: #000066; font-style: normal; text-decoration: underline;
                        font-weight: normal; }


                pre { font-family: 'DejaVu Sans Mono'; font-size: 9pt; margin-top: 5pt; margin-bottom: 5pt; }

                h1 {    font-weight: normal; font-size: 26pt; color: #000066;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 18pt; margin-bottom: 6pt;
                        border-top: 0.075cm solid #000000; border-bottom: 0.075cm solid #000000;
                        text-align: ; page-break-after:avoid; }
                h2 {    font-weight: bold; font-size: 12pt; color: #000066;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 6pt; margin-bottom: 6pt;
                        border-top: 0.07cm solid #000000; border-bottom: 0.07cm solid #000000;
                        text-align: ;  text-transform:uppercase; page-break-after:avoid; }
                h3 {    font-weight: normal; font-size: 26pt; color: #000000;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 0pt; margin-bottom: 6pt;
                        border-top: 0; border-bottom: 0;
                        text-align: ; page-break-after:avoid; }
                h4 {    font-weight: ; font-size: 13pt; color: #9f2b1e;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 10pt; margin-bottom: 7pt; font-variant: small-caps;
                        text-align: ;  margin-collapse:collapse; page-break-after:avoid; }
                h5 {    font-weight: bold; font-style:italic; ; font-size: 11pt; color: #000044;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 8pt; margin-bottom: 4pt;
                        text-align: ;  page-break-after:avoid; }
                h6 {    font-weight: bold; font-size: 9.5pt; color: #333333;
                        font-family: 'DejaVu Sans Condensed'; margin-top: 6pt; margin-bottom: ;
                        text-align: ;  page-break-after:avoid; }

                .breadcrumb {
                        text-align: right; font-size: 8pt; font-family: 'DejaVu Serif Condensed'; color: #666666;
                        font-weight: bold; font-style: normal; margin-bottom: 6pt; }

                .bpmTopic tbody tr:nth-child(even) { background-color: #f5f8f5; }
                .bpmTopicC tbody tr:nth-child(even) { background-color: #f5f8f5; }
                .bpmNoLines tbody tr:nth-child(even) { background-color: #f5f8f5; }
                .bpmNoLinesC tbody tr:nth-child(even) { background-color: #f5f8f5; }
                .bpmTopnTail tbody tr:nth-child(even) { background-color: #f5f8f5; }
                .bpmTopnTailC tbody tr:nth-child(even) { background-color: #f5f8f5; }

                .evenrow td, .evenrow th { background-color: #f5f8f5; }
                .oddrow td, .oddrow th { background-color: #e3ece4; }

                .bpmTopic {     background-color: #e3ece4; }
                .bpmTopicC { background-color: #e3ece4; }
                .bpmNoLines { background-color: #e3ece4; }
                .bpmNoLinesC { background-color: #e3ece4; }
                .bpmClear {             }
                .bpmClearC { text-align: center; }
                .bpmTopnTail { background-color: #e3ece4; topntail: 0.02cm solid #495b4a;}
                .bpmTopnTailC { background-color: #e3ece4; topntail: 0.02cm solid #495b4a;}
                .bpmTopnTailClear { topntail: 0.02cm solid #495b4a; }
                .bpmTopnTailClearC { topntail: 0.02cm solid #495b4a; }

                .bpmTopicC td, .bpmTopicC td p { text-align: center; }
                .bpmNoLinesC td, .bpmNoLinesC td p { text-align: center; }
                .bpmClearC td, .bpmClearC td p { text-align: center; }
                .bpmTopnTailC td, .bpmTopnTailC td p { text-align: center;  }
                .bpmTopnTailClearC td, .bpmTopnTailClearC td p {  text-align: center;  }

                .pmhMiddleCenter { text-align:center; vertical-align:middle; }
                .pmhMiddleRight {       text-align:right; vertical-align:middle; }
                .pmhBottomCenter { text-align:center; vertical-align:bottom; }
                .pmhBottomRight {       text-align:right; vertical-align:bottom; }
                .pmhTopCenter { text-align:center; vertical-align:top; }
                .pmhTopRight {  text-align:right; vertical-align:top; }
                .pmhTopLeft {   text-align:left; vertical-align:top; }
                .pmhBottomLeft {        text-align:left; vertical-align:bottom; }
                .pmhMiddleLeft {        text-align:left; vertical-align:middle; }

                .infobox { margin-top:10pt; background-color:#DDDDBB; text-align:center; border:1px solid #880000; }

                .bpmTopic td, .bpmTopic th  {   border-top: 1px solid #FFFFFF; }
                .bpmTopicC td, .bpmTopicC th  { border-top: 1px solid #FFFFFF; }
                .bpmTopnTail td, .bpmTopnTail th  {     border-top: 1px solid #FFFFFF; }
                .bpmTopnTailC td, .bpmTopnTailC th  {   border-top: 1px solid #FFFFFF; }

  </style>

</head>
<body>
<TABLE style="width: 197mm;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<TR>
<TD>Suplier:</TD>
<TD>{{ oper_name_short }}</TD>
</TR>
<TR>
<TD>Address:</TD>
<TD>
  {% if billing_address.line1 %}
    {{ billing_address.line1 }},
  {% endif %}
  {% if billing_address.line2 %}
    {{ billing_address.line2 }},
  {% endif %}
  {% if billing_address.line3 %}
    {{ billing_address.line3 }}
  {% endif %}
</TD>
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
<TD style="width: 43%; font-size: 9pt; vertical-align: bottom; text-align: center">Destination</TD>
<TD style="width: 5%; font-size: 9pt; vertical-align: bottom; text-align: center">Amount</TD>
</TR>
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
</TABLE>
<TABLE style="width: 100%;" BORDER="0" CELLPADDING="0" CELLSPACING="0">
<tr>
<td style="width: 85%; text-align: right;">Total:<td style="width: 15%; text-align: right;">${{ total_brutto }}</td>
</tr>
</TABLE>
</body>

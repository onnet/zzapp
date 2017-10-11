<!DOCTYPE html>
<head>
  <meta charset="UTF-8">
  <link href='http://fonts.googleapis.com/css?family=PT+Sans+Narrow&subset=latin,cyrillic' rel='stylesheet' type='text/css'>
  <style>

    * {
      font-family: 'PT Sans Narrow';
      font-size: 10pt;
    }

    table {
        width: 100%;
    }

    .footer_table {
      border-collapse: collapse;
    }

    .footer_table tr,
    .footer_table td {
      text-align: center;
      page-break-after:avoid;
    }

    .cell_padding {
      padding: 3px;
    }
  </style>
  <script>
    function pagination()
    {
      var vars = {};
      var x = document.location.search.substring(1).split('&');
      for (var i in x)
        {
                var z = x[i].split('=', 2);
                vars[z[0]] = unescape(z[1]);
        }
          var x = ['frompage','topage','page','webpage','section','subsection','subsubsection'];
          for (var i in x)
        {
                var y = document.getElementsByClassName(x[i]);
                  for (var j = 0; j < y.length; ++j)
                {
                        y[j].textContent = vars[x[i]];
                 }
        }
    }
  </script>
</head>

<body id="pdf-footer" onload="pagination()">
  <table class="footer_table">
    <tr>
      <td>
        ЗАО «ОнНет комьюникейшнс» 2017
      </td>
      <td>
        <span class="page"></span>
        /
        <span class="topage"></span>
      </td>
    </tr>
  </table>
</body>

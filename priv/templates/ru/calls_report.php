<?php
// $k_path_url='';
$html = file_get_contents($argv[1], "r");
include('/usr/local/mpdf/mpdf.php'); 
$mpdf=new mPDF('utf-8', 'A4',7,'verdana'); 
$stylesheet = file_get_contents('/usr/local/mpdf/examples/mpdfstyletables.css');
$mpdf->mirrorMargins = 0; // Use different Odd/Even headers and footers and mirror margins
$mpdf->defaultheaderfontsize = 7; /* in pts */
$mpdf->defaultheaderfontstyle = ''; /* blank, B, I, or BI */
$mpdf->defaultheaderline = 0; /* 1 to include line below header/above footer */
$mpdf->defaultfooterfontsize = 7; /* in pts */
$mpdf->defaultfooterfontstyle = ''; /* blank, B, I, or BI */
$mpdf->defaultfooterline = 0; /* 1 to include line below header/above footer */
$mpdf->SetHeader('ЗАО «ОнНет комьюникейшнс»||Детальный отчет о звонках');
// $mpdf->SetHeader('{DATE j-m-Y}|{PAGENO}/2|My document');
$mpdf->SetFooter('|Конфиденциально|{PAGENO}/{nbpg}'); /* defines footer for Odd and Even Pages - placed at Outer margin */
$mpdf->SetFooter(array(
'L' => array(
'content' => 'ЗАО «ОнНет комьюникейшнс»',
'font-family' => 'sans-serif',
'font-style' => 'B', /* blank, B, I, or BI */
'font-size' => '10', /* in pts */
),
'C' => array(
'content' => '- {PAGENO} -',
'font-family' => 'serif',
'font-style' => 'BI',
'font-size' => '18', /* gives default */
),
'R' => array(
'content' => 'Printed @ {DATE j-m-Y H:m}',
'font-family' => 'monospace',
'font-style' => '',
'font-size' => '10',
),
'line' => 1, /* 1 to include line below header/above footer */
), 'E' /* defines footer for Even Pages */
);
$mpdf->WriteHTML($stylesheet,1);
$mpdf->WriteHTML($html); 
$output = $argv[2];
$mpdf->Output($output,'F');
?>

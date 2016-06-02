<?php
$k_path_url='';
require_once('/usr/local/tcpdf/tcpdf.php');
$html = file_get_contents($argv[1], "r");
$pdf = new TCPDF('P', 'mm', 'A4', true, 'UTF-8', false);
$pdf->setPrintHeader(false);
$pdf->setPrintFooter(false);
$pdf->SetFont('verdana', '', 9);
$pdf->setImageScale(PDF_IMAGE_SCALE_RATIO);
$pdf->SetMargins(PDF_MARGIN_LEFT, PDF_MARGIN_TOP, PDF_MARGIN_RIGHT); 
$pdf->AddPage();
$pdf->writeHTML($html, true, 0, true, 0);

$pdf->Output($argv[2],'F');
?>

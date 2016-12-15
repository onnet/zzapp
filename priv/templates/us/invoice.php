<?php
$k_path_url='';
require_once('/usr/local/tcpdf/tcpdf.php');
$html = file_get_contents($argv[1], "r");
$pdf = new TCPDF('L', 'mm', 'A4', true, 'UTF-8', false);
$pdf->setPrintHeader(false);
$pdf->setPrintFooter(false); 
$pdf->SetFont('times', '', 10);
$pdf->setImageScale(PDF_IMAGE_SCALE_RATIO);
$pdf->SetMargins(PDF_MARGIN_LEFT, 5, PDF_MARGIN_RIGHT, -20); 
$pdf->AddPage();
$pdf->writeHTML($html, true, 0, true, 0);

$pdf->Output($argv[2],'F');
?>

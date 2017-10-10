<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>OnNet Innovations Limited</title>
   <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet"> 
  <!--   <link rel="stylesheet" href="css/bootstrap.css">  -->
    <style>
      @import url(https://fonts.googleapis.com/css?family=Special+Elite);
      body, h1, h2, h3, h4, h5, h6{
      font-family: 'Special Elite';
      }
    </style>
  </head>
  
  <body>
    <div class="container">
      <div class="row">
      <br />
      <br />
        <div class="col-xs-7">
          <h1 style="color: #E86110;">
            OnNet Innovations
          </h1>
        </div>
        <div class="col-xs-5 text-right">
          <h1><small style="color: black;">PRO-FORMA INVOICE</small></h1>
          <h3><small>#{{ invoicenumber }}</small></h3>
        </div>
      </div>
      <br />
      <br />
      <div class="row">
        <div class="col-xs-5">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>From: OnNet Innovations</h4>
            </div>
            <div class="panel-body">
              <p>
                Devonshire House<br>
                60 Goswell Road<br>
                London, EC1M 7AD<br>
              </p>
            </div>
          </div>
        </div>
        <div class="col-xs-5 col-xs-offset-2 text-left">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>To : {{ account_name }}</h4>
            </div>
            <div class="panel-body">
              <p>
                {{ billing_address.line1 }}<br>
                {{ billing_address.line2 }}<br>
                {{ billing_address.line3 }}<br>
              </p>
            </div>
          </div>
        </div>
      </div>
      <br />
      <br />
      <!-- / end client details section -->
      <table class="table table-bordered">
        <thead>
          <tr>
            <th>
              <h4>Description</h4>
            </th>
            <th>
              <h4>Hrs/Qty</h4>
            </th>
            <th>
              <h4>Rate/Price</h4>
            </th>
            <th>
              <h4>Sub Total</h4>
            </th>
          </tr>
        </thead>
        <tbody>
          {% for fee_line in monthly_fees %}
          <tr>
            <td>
              {{ fee_line.name }} {% if fee_line.period %}{{ fee_line.period }}.{{ fee_line.month_pad }}.{{ fee_line.year }}{% endif %}
            </td>
            <td class="text-right">
              {{ fee_line.quantity }}
            </td>
            <td class="text-right">
              {{ fee_line.rate_netto }}
            </td>
            <td class="text-right">
              {{ fee_line.cost_netto }}
            </td>
          </tr>
          {% endfor %}
        </tbody>
      </table>
      <br />
      <div class="row text-right">
        <div class="col-xs-4 col-xs-offset-6">
          <p>
            <strong>
            Total Net Amount : <br>
            VAT : <br>
            Invoice Total : <br>
            </strong>
          </p>
        </div>
        <div class="col-xs-2">
          <strong>
          {{ m.config.mod_kazoo.local_currency_sign.value }}{{ amount|format_price }} <br>
          N/A <br>
          {{ m.config.mod_kazoo.local_currency_sign.value }}{{ amount|format_price }} <br>
          </strong>
        </div>
      </div>
      <br />
      <br />
      <div class="row" style="page-break-inside: avoid !important;">
        <div class="col-xs-6">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>Bank details</h4>
            </div>
            <div class="panel-body">
              <p>OnNet Innovations Limited</p>
              <p>NATIONAL WESTMINSTER BANK PLC</p>
              <p>Sort Code : 56-00-23</p>
              <p>Account Number : 26045214</p>
              <p>IBAN : GB50NWBK56002326045214</p>
              <p>BIC : NWBKGB2L</p>
            </div>
          </div>
        </div>
        <div class="col-xs-6">
          <div class="span6">
            <div class="panel panel-info">
              <div class="panel-heading">
                <h4>Contact Details</h4>
              </div>
              <div class="panel-body">
                <p>
                  Email : accounts@onnet.info <br><br>
                  Telephone : +44 (0) 203 195 0326 <br> <br>
                  www : https://onnet.info
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>


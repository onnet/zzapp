<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>{{ carrier_vars.oper_name }}</title>
   <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet"> 
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
            {{ carrier_vars.oper_name }}
          </h1>
        </div>
        <div class="col-xs-5 text-right">
          <h1><small style="color: black;">PRO-FORMA INVOICE #{{ doc_pref }}{{ doc_number }}{{ doc_ind }}</small></h1>
          <h3><small>{{ doc_date_json.day }} {{ doc_date_json.month_short }} {{ doc_date_json.year }}</small></h3>
        </div>
      </div>
      <br />
      <br />
      <div class="row">
        <div class="col-xs-5">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>From: {{ carrier_vars.oper_name }}</h4>
            </div>
            <div class="panel-body">
              <p>
                {{ carrier_vars.billing_address.line1 }}<br>
                {{ carrier_vars.billing_address.line2 }}<br>
                {{ carrier_vars.billing_address.line3 }}<br>
              </p>
            </div>
          </div>
        </div>
        <div class="col-xs-5 col-xs-offset-2 text-left">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>To : {{ account_vars.account_name }}</h4>
            </div>
            <div class="panel-body">
              <p>
                {{ account_vars.billing_address.line1 }}<br>
                {{ account_vars.billing_address.line2 }}<br>
                {{ account_vars.billing_address.line3 }}<br>
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
            <th width="45%">
              <h4>Description</h4>
            </th>
            <th>
              <h4>Hrs/Qty</h4>
            </th>
            <th>
              <h4>Rate/Price</h4>
            </th>
            <th>
              <h4>Amount</h4>
            </th>
          </tr>
        </thead>
        <tbody>
          <tr style="page-break-inside: avoid !important;">
            <td>
              Account prepayment
            </td>
            <td class="text-right">
              -
            </td>
            <td class="text-right">
              {{ currency_sign }}{{ total_netto|floatformat:2 }}
            </td>
            <td class="text-right">
              {{ currency_sign }}{{ total_netto|floatformat:2 }}
            </td>
          </tr>
        </tbody>
      </table>
      <br />
      <div class="row text-right">
        <div class="col-xs-4 col-xs-offset-6">
          <p>
            <strong>
            Total Net Amount : <br>
            VAT ({{ vat_rate }}%) : <br>
            Invoice Total : <br>
            </strong>
          </p>
        </div>
        <div class="col-xs-2">
          <strong>
          {{ currency_sign }}{{ total_netto|floatformat:2 }} <br>
          {% if total_vat %}{{ currency_sign }}{{ total_vat|floatformat:2 }}{% else %}N/A{% endif %} <br>
          {{ currency_sign }}{{ total_brutto|floatformat:2 }} <br>
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
              <p>{{ carrier_vars.oper_name }}</p>
              <p>{{ carrier_vars.bank_details.line1 }}</p>
              <p>{{ carrier_vars.bank_details.line2 }}</p>
              <p>{{ carrier_vars.bank_details.line3 }}</p>
              {% if carrier_vars.bank_details.line4 %}
                <p>{{ carrier_vars.bank_details.line4 }}</p>
              {% endif %}
              {% if carrier_vars.bank_details.line5 %}
                <p>{{ carrier_vars.bank_details.line5 }}</p>
              {% endif %}
              {% if carrier_vars.bank_details.line6 %}
                <p>{{ carrier_vars.bank_details.line6 }}</p>
              {% endif %}
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
              {% if carrier_vars.contact_details.line1 %}
                <p>{{ carrier_vars.contact_details.line1 }}</p>
              {% endif %}
              {% if carrier_vars.contact_details.line2 %}
                <p>{{ carrier_vars.contact_details.line2 }}</p>
              {% endif %}
              {% if carrier_vars.contact_details.line3 %}
                <p>{{ carrier_vars.contact_details.line3 }}</p>
              {% endif %}
              {% if carrier_vars.contact_details.line4 %}
                <p>{{ carrier_vars.contact_details.line4 }}</p>
              {% endif %}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>


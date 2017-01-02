<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>{{ oper_name }}</title>
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
          <h3 style="color: #E86110;">
            {{ oper_name }}
          </h3>
        </div>
        <div class="col-xs-5 text-right">
          <h3><small style="color: black;">INVOICE #{{ doc_pref }}{{ doc_number }}{{ doc_ind }}</small></h3>
          <h4><small>{{ period_end.day }} {{ period_end.month_short }} {{ period_end.year }}</small></h4>
        </div>
      </div>
      <br />
      <br />
      <div class="row">
        <div class="col-xs-5">
          <div class="panel panel-info">
            <div class="panel-heading">
              <h4>From: {{ oper_name }}</h4>
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
              <h4>To : {{ account_name }}</h4>
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
        <h4>
          <small>
            Service period:
            {{ period_start.day }} {{ period_start.month_short }} {{ period_start.year }}
            -
            {{ period_end.day }} {{ period_end.month_short }} {{ period_end.year }}
          </small>
        </h4>
      <br />
      <!-- / end client details section -->
      <table class="table table-bordered">
        <thead>
          <tr>
            <th width="45%">
              <h4>Item</h4>
            </th>
            <th>
              <h4 style="text-align: center">Qty</h4>
            </th>
            <th>
              <h4 style="text-align: center">Rate</h4>
            </th>
            <th>
              <h4 style="text-align: center">Discount</h4>
            </th>
            <th>
              <h4 style="text-align: center">Amount</h4>
            </th>
          </tr>
        </thead>
        <tbody>
          {% for fee_line in monthly_fees %}
          <tr style="page-break-inside: avoid !important;">
            <td>
              {% if fee_line.type == "daily_calculated" %}
                {{ fee_line.name }}.
                {% for period in fee_line.period %}
                  {{ period.day }}
                  {{ period.month_short }}
                  {{ period.year }}{% if forloop.last %}.{% endif %}
                {% endfor %}
                Pro-rated.
              {% elif fee_line.category == "number_activation" %}
                Number activation: {{ fee_line.name }}. NRC.
              {% elif fee_line.category == "monthly_recurring" %}
                {{ fee_line.name }}. MRC.
              {% elif fee_line.category == "recurring_prorate" %}
                {{ fee_line.name }}. Prorated MRC.
                {% for period in fee_line.period %}
                  {{ period.day }}
                  {{ period.month_short }}
                  {{ period.year }}{% if forloop.last %}.{% endif %}
                {% endfor %}
              {% else %}
                {{ fee_line.name }}.
              {% endif %}
            </td>
            <td class="text-right">
              {{ fee_line.quantity }}
            </td>
            <td class="text-right">
              {{ currency_sign }}{{ fee_line.rate_netto }}
            </td>
            <td class="text-right">
              {{ currency_sign }}{{ fee_line.discount_netto }}
            </td>
            <td class="text-right">
              {{ currency_sign }}{{ fee_line.discounted_cost_netto }}
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
            Subtotal : <br>
            Sales Tax ({{ vat_rate }}%) : <br>
            Invoice Total : <br>
            </strong>
          </p>
        </div>
        <div class="col-xs-2">
          <strong>
          {{ currency_sign }}{{ total_netto|floatformat:2 }} <br>
          {{ currency_sign }}{{ total_vat|floatformat:2 }} <br>
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
              <p>{{ oper_name }}</p>
              <p>{{ bank_details.line1 }}</p>
              <p>{{ bank_details.line2 }}</p>
              <p>{{ bank_details.line3 }}</p>
              {% if bank_details.line4 %}
                <p>{{ bank_details.line4 }}</p>
              {% endif %}
              {% if bank_details.line5 %}
                <p>{{ bank_details.line5 }}</p>
              {% endif %}
              {% if bank_details.line6 %}
                <p>{{ bank_details.line6 }}</p>
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
              {% if contact_details.line1 %}
                <p>{{ contact_details.line1 }}</p>
              {% endif %}
              {% if contact_details.line2 %}
                <p>{{ contact_details.line2 }}</p>
              {% endif %}
              {% if contact_details.line3 %}
                <p>{{ contact_details.line3 }}</p>
              {% endif %}
              {% if contact_details.line4 %}
                <p>{{ contact_details.line4 }}</p>
              {% endif %}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>


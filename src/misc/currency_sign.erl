-module(currency_sign).
-export([add_currency_sign/2
        ,add_account_currency_sign/2
        ]).

-include("onbill.hrl").

-spec add_currency_sign(ne_binary(), number()) -> ne_binary().
add_currency_sign(CurrencySign, Amount) ->
    case (Amount >= -0.0049) of
        'true' ->
             <<(kz_term:to_binary(CurrencySign))/binary, (format_price:format_price(abs(Amount)))/binary>>;
        'false' ->
             <<"- ", (kz_term:to_binary(CurrencySign))/binary, (format_price:format_price(abs(Amount)))/binary>>
    end.

-spec add_account_currency_sign(ne_binary(), number()) -> ne_binary().
add_account_currency_sign(AccountId, Amount) ->
    ResellerVars = onbill_util:reseller_vars(AccountId),
    add_currency_sign(kz_json:get_value(<<"currency_sign">>, ResellerVars, <<"£"/utf8>>), Amount).

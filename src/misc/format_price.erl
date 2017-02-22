-module(format_price).
-export([format_price/1, format_price/2, format_price/3]).

insert_thousands_separator(_Sep, Output, []) ->
    Output;

insert_thousands_separator(Sep, Output, Input) when is_list(Input) ->
    case length(Input) > 3 of
        true ->
            case length(Input) rem 3 of
                0 -> 
                     [Head | Input2] = Input,
                     insert_thousands_separator(Sep, lists:append(Output, [Head]), Input2);
                1 -> 
                     [Head | Input2] = Input,
                     Head1 = lists:append([Head], [Sep]),
                     insert_thousands_separator(Sep, lists:append(Output, Head1), Input2);
                2 ->
                     [Head | Input2] = Input,
                     insert_thousands_separator(Sep, lists:append(Output, [Head]), Input2)
            end;
        false -> lists:append(Output, Input)
    end.

insert_thousands_separator(Sep, Input) when is_integer(Input) ->
    insert_thousands_separator(Sep, [], integer_to_list(Input)).

-spec format_price(any()) -> any().
-spec format_price(any(), any()) -> any().
-spec format_price(any(), any(), any()) -> any().
format_price(Input, DSep, TSep) when is_integer(Input), Input < 0 ->
    iolist_to_binary([$-, format_price(-Input, DSep, TSep)]);
format_price(Input, DSep, TSep) when is_integer(Input) ->
    case Input rem 100 of
        0 -> 
            iolist_to_binary([insert_thousands_separator(TSep, Input div 100), DSep, $0, $0 ]);
        Cents when Cents < 10 -> 
            iolist_to_binary([insert_thousands_separator(TSep, Input div 100), DSep, $0, Cents + $0 ]);
        Cents -> 
            iolist_to_binary([insert_thousands_separator(TSep, Input div 100), DSep, integer_to_list(Cents) ])
    end;
format_price(Input, DSep, TSep) when is_float(Input) ->
    format_price(round(Input * 100), DSep, TSep);
format_price(undefined, _Dsep, _Tsep) ->
    undefined;
format_price(<<>>, _Dsep, _Tsep) ->
    undefined;
format_price([], _Dsep, _Tsep) ->
    undefined;
format_price(Input, DSep, TSep) ->
    format_price(kz_term:to_integer(Input), DSep, TSep).

format_price(Input, Args) ->
    case length(Args) of
        0 -> format_price(Input, $., $,);
        1 -> format_price(Input, Args, $,);
        2 -> [DSep, TSep] = Args,
             format_price(Input, DSep, TSep);
        _ -> format_price(Input, $., $,)
    end.

format_price(Input) ->
    format_price(Input, $., $,).

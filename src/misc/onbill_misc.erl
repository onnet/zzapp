-module(onbill_misc).
-author("Kirill Sysoev <kirill.sysoev@gmail.com>").

-export([translit/1
]).

-type(utf8_str() :: [integer()]).
-type(ascii_str() :: [byte()]).

-spec(translit(utf8_str()) -> ascii_str()).
translit(Data) when is_binary(Data) ->
    translit(kz_term:to_list(Data));
translit(Data) ->
    translit(Data, []).

-spec(translit(utf8_str(), list()) -> ascii_str()).
translit([], Acc) -> lists:flatten(lists:reverse(Acc));
translit([Last], Acc) -> lists:flatten(lists:reverse([Last | Acc]));
translit([C1, C2 | Rest], Acc) ->
    case proplists:get_value({C1, C2}, translit_map()) of
        undefined -> translit([C2 | Rest], [C1 | Acc]);
        {Str, _} -> translit(Rest, [Str | Acc])
    end.

translit_map() ->
    L1 = [{208, N} || N <- lists:seq(144, 191)],
    L2 = [{209, N} || N <- lists:seq(128, 143)],
    D1 = [{"A", "А"}, {"B", "Б"}, {"V", "В"}, {"G", "Г"}, {"D", "Д"}, {"E", "Е"},
          {"Zh", "Ж"}, {"Z", "З"}, {"I", "И"}, {"J", "Й"}, {"K", "К"}, {"L", "Л"},
          {"M", "М"}, {"N", "Н"}, {"O", "О"}, {"P", "П"}, {"R", "Р"}, {"S", "С"},
          {"T", "Т"}, {"U", "У"}, {"F", "Ф"}, {"H", "Х"}, {"Ts", "Ц"}, {"Ch", "Ч"},
          {"Sh", "Ш"}, {"Shh","Щ"}, {"#", "Ъ"}, {"Y", "Ы"}, {"'", "Ь"}, {"Je", "Э"},
          {"Ju", "Ю"}, {"Ja", "Я"},
          {"a", "а "}, {"b", "б"}, {"v", "в"}, {"g", "г"}, {"d", "д"}, {"e", "е"},
          {"zh", "ж"}, {"z", "з"}, {"i", "и"}, {"j", "й"}, {"k", "к"}, {"l", "л"},
          {"m", "м"}, {"n", "н"}, {"o", "о"}, {"p", "п"}],
    D2 = [{"r", "р"}, {"s", "с"}, {"t", "т"}, {"u", "у"}, {"f", "ф"}, {"h", "х"},
          {"ts", "ц"}, {"ch", "ч"}, {"sh", "ш"}, {"shh","щ"}, {"#", "ъ"}, {"y", "ы"},
          {"'", "ь"}, {"je", "э"}, {"ju", "ю"}, {"ja", "я"}],
    [{{208, 129}, {"Yo", "Ё"}},
     {{209, 145}, {"yo", "ё"}},
     {{194, 171}, {"\"", "«"}},
     {{194, 187}, {"\"", "»"}}
    ] ++ lists:zip(L1, D1) ++ lists:zip(L2, D2).

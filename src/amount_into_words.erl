-module(amount_into_words).

-export([render/1]).

-define(U(A), unicode:characters_to_list(A, utf8)).

render(Num) when is_integer(Num) ->
    Tris = tri_split(lists:reverse(integer_to_list(Num)), [], [], 0),
    gen_string(Tris, length(Tris) + 1, []).

gen_string([], _, Acc) ->
    string:strip(Acc);
gen_string([H|T], Index, Acc) ->
    case list_to_integer(H) of
        0 ->
            gen_string(T, Index-1, Acc);
        _Digit ->
            {IndexGender, IndexClass} = get_index_class_gender(H, Index),
            gen_string(T, Index-1, 
                string:join([Acc, 
                        tri_to_text(H, IndexGender), 
                        local_index(Index, IndexClass)],
                    " "))
    end.

get_index_class_gender(Num, Index) ->
    Int = list_to_integer(Num),
    Class = case {Int rem 100, Int rem 10} of
        {N, _} when N > 4, N < 21 ->
            3;
        {_, 1} ->
            1;
        {_, M} when M > 1, M < 5 ->
            2;
        {_, _} ->
            3
    end,
    Gender = case Index of
        3 ->
            false;
        _ ->
            true
    end,
    {Gender, Class}.

tri_split([], Acc, Buf, _Num) ->
    [Buf|Acc];
tri_split([N|O], Acc, Buf, Num) when Num < 3 ->
    tri_split(O, Acc, [N|Buf], Num + 1);
tri_split([_N|_A] = A, Acc, Buf, 3) ->
    tri_split(A, [Buf|Acc], [], 0).

tri_to_text("0", _) ->
    [];
tri_to_text(Tri, Gender) ->
    Int = list_to_integer(Tri),
    string:join(lists:filter(fun(X) -> X /= [] end, [tri_check(100, Int rem 1000)|
        tri_check(11, Int rem 100, Int, Gender)]), " ").

tri_check(100, N) when N > 99 ->
    local_100(N div 100);
tri_check(10, N) when N == 10; N > 19 ->
    local_10(N div 10);
tri_check(_, _) ->
    [].

tri_check(11, N, _, _) when N > 10, N < 20 ->
    [local_11(N)];
tri_check(11, N, Int, Gender) ->
    [tri_check(10, N),
    tri_check(1, Int rem 10, Gender)].

tri_check(1, N, Gender) when N > 0 ->
    local_1(N, Gender);
tri_check(_, _, _) ->
    [].

% russian
local_1(1, true) -> ?U("один"); 
local_1(1, false) -> ?U("одна");
local_1(2, false) -> ?U("две");
local_1(2, true) -> ?U("два");
local_1(3, _) -> ?U("три");
local_1(4, _) -> ?U("четыре");
local_1(5, _) -> ?U("пять");
local_1(6, _) -> ?U("шесть");
local_1(7, _) -> ?U("семь");
local_1(8, _) -> ?U("восемь");
local_1(9, _) -> ?U("девять");
local_1(_, _) -> [].

local_10(1) -> ?U("десять");
local_10(2) -> ?U("двадцать");
local_10(3) -> ?U("тридцать");
local_10(4) -> ?U("сорок");
local_10(5) -> ?U("пятьдесят");
local_10(6) -> ?U("шестьдесят");
local_10(7) -> ?U("семьдесят");
local_10(8) -> ?U("восемьдесят");
local_10(9) -> ?U("девяносто").


local_100(1) -> ?U("сто");
local_100(2) -> ?U("двести");
local_100(3) -> ?U("триста");
local_100(4) -> ?U("четыреста");
local_100(5) -> ?U("пятьсот");
local_100(6) -> ?U("шестьсот");
local_100(7) -> ?U("семьсот");
local_100(8) -> ?U("восемьсот");
local_100(9) -> ?U("девятьсот").

local_11(11) -> ?U("одиннадцать");
local_11(12) -> ?U("двенадцать");
local_11(13) -> ?U("тринадцать");
local_11(14) -> ?U("четырнадцать");
local_11(15) -> ?U("пятнадцать");
local_11(16) -> ?U("шестнадцать");
local_11(17) -> ?U("семнадцать");
local_11(18) -> ?U("восемнадцать");
local_11(19) -> ?U("девятнадцать").

local_index(1, _) -> [];
local_index(2, _) -> [];
local_index(3, 1) -> ?U("тысяча");
local_index(3, 2) -> ?U("тысячи");
local_index(3, 3) -> ?U("тысяч");
local_index(4, 1) -> ?U("миллион");
local_index(4, 2) -> ?U("миллиона");
local_index(4, 3) -> ?U("миллионов");
local_index(5, 1) -> ?U("миллиард");
local_index(5, 2) -> ?U("миллиарда");
local_index(5, 3) -> ?U("миллиардов");
local_index(6, 1) -> ?U("триллион");
local_index(6, 2) -> ?U("триллиона");
local_index(6, 3) -> ?U("триллионов").

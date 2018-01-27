-module(onbill_dtl_lib).
-behaviour(erlydtl_library).

-export([version/0, inventory/1]).

-export([pad_left/1
        ,gregsec_to_date/1
        ]).

-include_lib("tasks/src/tasks.hrl").

-spec version() -> 1.
version() -> 1.

-spec inventory(atom()) -> kz_proplist().
inventory(filters) -> [pad_left, gregsec_to_date];
inventory(tags) -> [].

-spec pad_left(any()) -> ne_binary().
pad_left(Val) ->
    kz_binary:pad_left(kz_term:to_binary(Val), 2, <<"0">>).

-spec gregsec_to_date(any()) -> kz_datetime().
gregsec_to_date(Seconds) ->
    try
        calendar:gregorian_seconds_to_datetime(Seconds)
    catch
        _ -> Seconds
    end.


%% good example: http://www.jonasrichard.com/2016/01/templating-with-erlydtl-1.html
%%{% get_price id=product.id as price %}
%get_price(Vars, _Opts) ->
%    case lists:keyfind(id, 1, Vars) of
%        {id, Id} ->
%            %% Let it crash if service fails
%            {ok, Price} = guitar_store:get_price_by_id(Id),
%            [{value, Price}];
%        false ->
%            %% No id specified, we can crash or we can
%            %% leave the context variables as they are
%            []
%    end.

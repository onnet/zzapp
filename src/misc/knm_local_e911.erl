%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% Dummy e911 provisioning
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(knm_local_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include_lib("kazoo_number_manager/src/knm.hrl").

-define(ADDRESS_ID, <<"address_id">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    update_e911(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            {'ok', NewNumber} = remove_number(Number),
            knm_services:deactivate_feature(NewNumber, ?FEATURE_E911)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?FEATURE_E911).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_e911(knm_number:knm_number()) -> knm_number:knm_number().
update_e911(Number) ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_services:deactivate_feature(Number, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_services:activate_feature(Number, {?FEATURE_E911, E911})
    end.

-spec remove_number(knm_number:knm_number()) -> {'ok', knm_number:knm_number()} |
                                                {'error', ne_binary()}.
remove_number(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> {'ok', Number};
        AddressId ->
            lager:debug("removing previously set address: ~p", [AddressId]),
            assign_address(Number, 'null')
    end.

-spec assign_address(knm_number:knm_number(), ne_binary() | 'null') -> {'ok', knm_number:knm_number()}.
assign_address(Number, AddressId) ->
    {'ok', set_address_id(Number, AddressId)}.

-spec set_address_id(knm_number:knm_number(), ne_binary() | 'null') -> knm_number:knm_number().
set_address_id(Number, AddressId) ->
    PN = knm_number:phone_number(Number),
    Data = kz_json:from_list([{?ADDRESS_ID, AddressId}]),
    NewPN = knm_phone_number:update_carrier_data(PN, Data),
    knm_number:set_phone_number(Number, NewPN).


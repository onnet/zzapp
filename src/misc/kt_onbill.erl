%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_onbill).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ]).

%% Verifiers
-export([
        ]).

%% Appliers
-export([current_state/2
        ]).

-include_lib("tasks/src/tasks.hrl").
-include_lib("kazoo_services/include/kz_service.hrl").

-define(CATEGORY, "onbill").
-define(ACTIONS, [<<"current_state">>
                 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).


-spec output_header(ne_binary()) -> kz_csv:row().
output_header(<<"current_state">>) ->
    [<<"account_id">>
    ,<<"year">>
    ,<<"month">>
    ,<<"category">>
    ,<<"item">>
    ,<<"quantity_bom">>
    ,<<"quantity_eom">>
    ].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_proplist().
action(<<"current_state">>) ->
    [{<<"description">>, <<"List per-month descendant accounts quantities">>}
    ,{<<"doc">>, <<"Line1.\n"
                   "Line 2.\n"
                 >>}
    ].

%%% Verifiers


%%% Appliers

-spec current_state(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
current_state(#{account_id := AccountId}, init) ->
    Descendants = get_descendants(AccountId),
    DescendantsMoDBs = lists:flatmap(fun kapps_util:get_account_mods/1, Descendants),
    lager:debug("found ~p descendants & ~p MoDBs in total"
               ,[length(Descendants), length(DescendantsMoDBs)]),
    {ok, DescendantsMoDBs};
current_state(_, []) -> stop;
current_state(_, [SubAccountMoDB | DescendantsMoDBs]) ->
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, YYYY, MM) = SubAccountMoDB,
    AccountId = ?MATCH_ACCOUNT_RAW(A, B, Rest),
    BoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_BOM),
    EoM = modb_service_quantities(SubAccountMoDB, ?SERVICES_EOM),
    case rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) of
        [] ->
            %% No rows generated: ask worker to skip writing for this step.
            {ok, DescendantsMoDBs};
        Rows -> {Rows, DescendantsMoDBs}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec rows_for_quantities(ne_binary(), ne_binary(), ne_binary(), kz_json:object(), kz_json:object()) ->
                                 [kz_csv:row()].
rows_for_quantities(AccountId, YYYY, MM, BoM, EoM) ->
    lists:append(
      [quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem)
       || Category <- fields(BoM, EoM),
          BoMItem <- [kz_json:get_value(Category, BoM)],
          EoMItem <- [kz_json:get_value(Category, EoM)]
      ]).

-spec quantities_for_items(ne_binary(), ne_binary(), ne_binary(), ne_binary(), api_object(), api_object()) ->
                                  [kz_csv:row()].
quantities_for_items(AccountId, YYYY, MM, Category, BoMItem, EoMItem) ->
    [ [AccountId
      ,YYYY
      ,MM
      ,Category
      ,Item
      ,maybe_integer_to_binary(Item, BoMItem)
      ,maybe_integer_to_binary(Item, EoMItem)
      ]
      || Item <- fields(BoMItem, EoMItem)
    ].

-spec get_descendants(ne_binary()) -> ne_binaries().
get_descendants(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.

-spec modb_service_quantities(ne_binary(), ne_binary()) -> kz_json:object().
modb_service_quantities(MoDB, Id) ->
    case kz_datamgr:open_doc(MoDB, Id) of
        {'ok', JObj} -> kz_json:get_value(<<"quantities">>, JObj);
        {'error', _R} ->
            lager:debug("could not fetch ~s in modb ~s: ~p", [Id, MoDB, _R]),
            kz_json:new()
    end.

-spec fields(api_object(), api_object()) -> ne_binaries().
fields('undefined', JObjB) ->
    fields(kz_json:new(), JObjB);
fields(JObjA, 'undefined') ->
    fields(JObjA, kz_json:new());
fields(JObjA, JObjB) ->
    lists:usort(kz_json:get_keys(JObjA) ++ kz_json:get_keys(JObjB)).

-spec maybe_integer_to_binary(ne_binary(), api_object()) -> api_non_neg_integer().
maybe_integer_to_binary(_, 'undefined') -> 'undefined';
maybe_integer_to_binary(Item, JObj) ->
    case kz_json:get_integer_value(Item, JObj) of
        'undefined' -> 'undefined';
        Quantity -> integer_to_binary(Quantity)
    end.


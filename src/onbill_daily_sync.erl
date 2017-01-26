-module(onbill_daily_sync).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("onbill.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(CB_LISTING_BY_ID, <<"accounts/listing_by_id">>).

-record(state, {}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {'ok', state()}.
init([]) ->
    self() ! 'crawl_accounts',
    kz_couch_compactor:compact_db(<<"services">>),
    {'ok', #state{}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('crawl_accounts', _) ->
    _ = case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?CB_LISTING_BY_ID) of
            {'ok', JObjs} ->
                self() ! 'next_account',
                {'noreply', kz_util:shuffle_list(JObjs)};
            {'error', _R} ->
                lager:warning("unable to list all docs in ~s: ~p", [?KZ_ACCOUNTS_DB, _R]),
                self() ! 'next_account',
                {'noreply', []}
        end;
handle_info('next_account', []) ->
    kz_couch_compactor:compact_db(<<"services">>),
    NextDay = calendar:datetime_to_gregorian_seconds({erlang:date(),{0,45,0}}) + ?SECONDS_IN_DAY,
    Cycle = NextDay - calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    erlang:send_after(Cycle, self(), 'crawl_accounts'),
    {'noreply', [], 'hibernate'};
handle_info('next_account', [Account|Accounts]) ->
    _ = case kz_doc:id(Account) of
            <<"_design", _/binary>> -> 'ok';
            AccountId ->
                OpenResult = kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId),
                check_then_process_account(AccountId, OpenResult)
        end,
    Cycle = kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<"interaccount_delay">>, 10 * ?MILLISECONDS_IN_SECOND),
    erlang:send_after(Cycle, self(), 'next_account'),
    {'noreply', Accounts, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_then_process_account(ne_binary(), {'ok', kz_account:doc()} | {'error',any()}) -> 'ok'.
check_then_process_account(AccountId, {'ok', AccountJObj}) ->
    case kz_doc:is_soft_deleted(AccountJObj) of
        'true' -> 'ok'; 
        'false' ->
            process_account(AccountId)
    end;
check_then_process_account(AccountId, {'error', _R}) ->
    lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R]).

-spec process_account (ne_binary()) -> 'ok'.
process_account(AccountId) ->
    lager:debug("onbill crawler syncing account ~s", [AccountId]),
    _ = kz_services:reconcile(AccountId),
    kz_service_sync:sync(AccountId),
    'ok'.

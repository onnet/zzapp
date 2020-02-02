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
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(CB_LISTING_BY_ID, <<"accounts/listing_by_id">>).

-record(state, {}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {'ok', state()}.
init([]) ->
    self() ! 'crawl_accounts',
 %   kt_compactor:compact_db(<<"services">>),
    {'ok', #state{}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('crawl_accounts', _) ->
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?CB_LISTING_BY_ID) of
        {'ok', JObjs} ->
            self() ! 'next_account',
            {'noreply', kz_term:shuffle_list(JObjs)};
        {'error', _R} ->
            lager:warning("unable to list all docs in ~s: ~p", [?KZ_ACCOUNTS_DB, _R]),
            self() ! 'next_account',
            {'noreply', []}
    end;
handle_info('next_account', []) ->
 %   kz_couch_compactor:compact_db(<<"services">>),
    erlang:send_after(?MILLISECONDS_IN_HOUR, self(), 'crawl_accounts'),
    {'noreply', [], 'hibernate'};
handle_info('next_account', [Account|Accounts]) ->
    Cycle =
        case maybe_process_account(kz_doc:id(Account)) of
            {'ok', 'account_processed'} ->
                kapps_config:get_integer(?MOD_CONFIG_CRAWLER, <<"interaccount_delay_sec">>, 10)  * ?MILLISECONDS_IN_SECOND;
            {'ok', 'no_need_to_process'} ->
                2 * ?MILLISECONDS_IN_SECOND;
            {'error', _} ->
                2 * ?MILLISECONDS_IN_SECOND
        end,
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

-spec maybe_process_account (kz_term:ne_binary()) -> {'ok', 'account_processed'}|{'ok', 'no_need_to_process'}|{'error', _}.
maybe_process_account(<<AccountId:32/binary>>) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', AccountJObj} ->
            case kz_doc:is_soft_deleted(AccountJObj)
                orelse not kzd_accounts:is_enabled(AccountJObj)
                orelse not kz_datamgr:db_exists(kz_util:format_account_id(AccountId, 'encoded'))
            of
                'false' ->
                    process_account(AccountId, AccountJObj);
                'true' ->
                    {'error', 'deleted_or_no_db'}
            end;
        _ ->
            {'error', 'invalid_accounts_doc'}
    end;
maybe_process_account(_) ->
    {'error', 'invalid_doc_id'}.

-spec process_account (kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
process_account(AccountId, AccountJObj) ->
    case  not kapps_util:is_master_account(AccountId) 
           andalso zz_util:is_service_plan_assigned(AccountId)
           andalso onbill_bk_util:today_dailyfee_absent(AccountId)
    of
        'true' ->
            lager:debug("crawler - saving account ~p (~p) as dirty", [AccountId, kzd_accounts:name(AccountJObj)]),
            case zz_util:current_service_status(AccountId) of
                <<"delinquent">> ->
                    ProcessNewPeriod = zz_util:maybe_process_new_billing_period(AccountId),
                    maybe_remove_subscriptions(AccountId, ProcessNewPeriod);
                _ ->
                    zz_util:maybe_save_as_dirty(AccountId),
                    onbill_notifications:maybe_send_account_updates(AccountId, AccountJObj)
            end,
            {'ok', 'account_processed'};
        'false' ->
            lager:debug("crawler - no need to process account ~p (~p)", [AccountId, kzd_accounts:name(AccountJObj)]),
            {'ok', 'no_need_to_process'}
    end.

-spec maybe_remove_subscriptions(kz_term:ne_binary(), boolean()) -> any().
maybe_remove_subscriptions(AccountId, 'true') ->
    onbill_bk_util:maybe_cancel_trunk_subscriptions(AccountId);
maybe_remove_subscriptions(_, 'false') ->
    'ok'.

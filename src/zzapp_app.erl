%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(zzapp_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    onbill_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
lager:debug("IAM start zzapp_app"),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_conf:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_money:declare_exchanges(),
    kapi_self:declare_exchanges().

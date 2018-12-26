%%%-------------------------------------------------------------------
%% @doc account service public API
%% @end
%%%-------------------------------------------------------------------

-module(account_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/bankaccounts", account_rest_handler, all},
               {"/bankaccounts/default", account_rest_handler, default}]}
    ]),
    {ok, _} = cowboy:start_clear(account_http_listener, [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    account_service_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

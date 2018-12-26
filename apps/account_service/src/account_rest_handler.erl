-module(account_rest_handler).
-behaviour(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, content_to_json}
    ], Req, State}.

content_to_json(Req, Account) ->
    Accounts = account_data:fetch_accounts(),
    Content = case Account of
                  all -> Accounts;
                  default -> case account_data:get_default(Accounts) of
                                 {ok, Id} -> Id;
                                 false -> null
                             end
              end,
    Opts = [pretty || application:get_env(account_service, pretty_output, false)],
    {jiffy:encode(Content, [uescape, force_utf8 | Opts]), Req, Account}.
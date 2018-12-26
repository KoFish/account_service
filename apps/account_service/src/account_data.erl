%%%-------------------------------------------------------------------
%% @doc account service data management module
%% @end
%%%-------------------------------------------------------------------

-module(account_data).

%% API
-export([fetch_accounts/0]).
-export([get_default/1]).

%%====================================================================
%% Types
%%====================================================================

-type account_id() :: integer().
-type account() :: #{id => account_id(),
                     name => binary(),
                     number => binary(),
                     creditcard => boolean(),
                     synthetic => boolean(),
                     balance => float()}.

%%====================================================================
%% API
%%====================================================================

-spec fetch_accounts() -> [account()].
fetch_accounts() ->
    {ok, Content} = file:read_file("bankaccounts.json"),
    Accounts = jiffy:decode(Content, [return_maps]),
    [maps:fold(fun key_to_atom/3, #{}, Account) || Account <- Accounts].

-spec get_default([account()]) -> {ok, account_id()} | false.
get_default(Accounts) ->
    case lists:sort(fun sort_by_balance/2, Accounts) of
        [#{id := IdA, balance := BalanceA} = AccountA, #{balance := BalanceB} | _] ->
            case BalanceA >= (BalanceB * 2) andalso is_valid_default(AccountA) of
               true -> {ok, IdA};
               false -> false
            end;
        [#{id := Id} = Account] ->
            case is_valid_default(Account) of
                true -> {ok, Id};
                false -> false
            end;
        _ ->
            false
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec is_valid_default(account()) -> boolean().
is_valid_default(#{synthetic := true}) -> false;
is_valid_default(#{balance := Balance}) when Balance < 0 -> false;
is_valid_default(#{}) -> true.

-spec sort_by_balance(account(), account()) -> boolean().
sort_by_balance(#{balance := A}, #{balance := B}) -> A > B.

-spec key_to_atom(binary(), any(), map()) -> map().
key_to_atom(Key, Value, Acc) -> maps:put(erlang:binary_to_atom(Key, utf8), Value, Acc).

%%====================================================================
%% Module specific unit tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

account(Id, Number, Name, Creditcard, Synthetic, Balance) ->
    #{id => Id,
      name => Name,
      number => Number,
      creditcard => Creditcard,
      synthetic => Synthetic,
      balance => Balance}.

balance_sort_test_() ->
    [
     fun () ->
        Test = [],
        [] = lists:sort(fun sort_by_balance/2, Test)
     end,
     fun () ->
        Test = [account(1, <<"">>, <<"">>, false, false, 0)],
        [#{id := 1}] = lists:sort(fun sort_by_balance/2, Test)
     end,
     fun () ->
        Test = [account(1, <<"">>, <<"">>, false, false, 3.0),
                account(1, <<"">>, <<"">>, false, false, -3.0),
                account(1, <<"">>, <<"">>, false, false, 5.4),
                account(1, <<"">>, <<"">>, false, false, 3.0)
               ],
        [#{balance := 5.4},
         #{balance := 3.0},
         #{balance := 3.0},
         #{balance := -3.0}] = lists:sort(fun sort_by_balance/2, Test)
     end,
     fun () ->
        Test = [account(10, <<"123">>, <<"Account 1">>, false, false, 300.0),
                account(2, <<"">>, <<"Account 2">>, false, false, -100.0),
                account(33, <<"789">>, <<"Account 3">>, false, false, -157.0),
                account(400, <<"456">>, <<"Account 4">>, false, false, 10.0)
               ],
        [#{id := 10},
         #{id := 400},
         #{id := 2},
         #{id := 33}] = lists:sort(fun sort_by_balance/2, Test)
     end
    ].

valid_test_() ->
    [?_assert(is_valid_default(account(1, <<"111">>, <<"Valid default">>, false, false, 100.00))),
     ?_assertNot(is_valid_default(account(2, <<"121">>, <<"Invalid default">>, false, true, 100.00))),
     ?_assert(is_valid_default(account(3, <<"131">>, <<"Valid default">>,  true, false, 100.00))),
     ?_assertNot(is_valid_default(account(4, <<"141">>, <<"Invalid default">>, false, false, -100.00)))
    ].

default_test_() ->
    [
     fun () ->
        false = get_default([])
     end,
     fun () ->
        %% Synthetic isn't allowed as default
        false = get_default([account(1, <<"">>, <<"">>, false, true, 600.0)])
     end,
     fun () ->
        %% Negative balance isn't allowed as default
        false = get_default([account(1, <<"">>, <<"">>, false, false, -600.0)])
     end,
     fun () ->
        %% Single account is the default
        {ok, 1} = get_default([account(1, <<"">>, <<"">>, false, false, 600.0)])
     end,
     fun () ->
        %% If the account with highest balance isn't twice as high it can't be default
        false = get_default([account(1, <<"">>, <<"">>, false, false, 10.0),
                             account(2, <<"">>, <<"">>, false, false, 10.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high it is the default
        {ok, 2} = get_default([account(1, <<"">>, <<"">>, false, false, 10.0),
                               account(2, <<"">>, <<"">>, false, false, 20.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high it is the default unless it's synthethic
        false = get_default([account(1, <<"">>, <<"">>, false, false, 10.0),
                             account(2, <<"">>, <<"">>, false, true, 20.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high it is the default unless it's negative balance
        false = get_default([account(1, <<"">>, <<"">>, false, false, -10.0),
                             account(2, <<"">>, <<"">>, false, false, -20.0)])
     end,
     fun () ->
        %% If the account with highest balance isn't twice as high as all others it can't be default
        false = get_default([account(1, <<"">>, <<"">>, false, false, 5.0),
                             account(2, <<"">>, <<"">>, false, false, 20.0),
                             account(3, <<"">>, <<"">>, false, false, 30.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high as all others it can't be default if it's synthetic
        false = get_default([account(1, <<"">>, <<"">>, false, false, 5.0),
                             account(2, <<"">>, <<"">>, false, false, 10.0),
                             account(3, <<"">>, <<"">>, false, true, 30.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high as all others it can be default if it isn't synthetic
        {ok, 2} = get_default([account(1, <<"">>, <<"">>, false, false, 5.0),
                               account(2, <<"">>, <<"">>, false, false, 30.0),
                               account(3, <<"">>, <<"">>, false, false, 10.0)])
     end,
     fun () ->
        %% If the account with highest balance is twice as high as all others but negative it can't be default
        false = get_default([account(1, <<"">>, <<"">>, false, false, -5.0),
                             account(2, <<"">>, <<"">>, false, false, -30.0),
                             account(3, <<"">>, <<"">>, false, false, -10.0)])
     end
    ].

-endif.
%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc
%%% Include file for the lobby module.
%%% @end
%%% Created : 11 Apr 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------

%% Definitions
-define(MAX_PLAYERS, 6).
-define(TIMEOUT, 5000).

%% Types
-type player_id() :: 1..6.
-type peg_id() :: 1..121.
-type board() :: digraph().

%% Records
-record(player, {id :: player_id(),
                 name :: string(),
                 ppid :: pid(),
                 st = idle :: 'idle' | {'in_game', string()} |
                              {'spectating', string()}
                }).

-record(game, {name :: string(),
               host :: pid(),
               id :: pos_integer(),
               gpid :: pid(),
               st = waiting :: 'waiting' | 'started',
               pls = [] :: list(#player{}),
               specs = [] :: list(#player{}),
               num = 1 :: 0..6,
               board :: board()
              }).

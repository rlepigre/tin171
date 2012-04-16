%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc
%%% Top-level supervisor for chinese checkers server
%%% @end
%%% Created : 30 Mar 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(cc_sup).
-behaviour(supervisor).
-compile([debug_info]).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialization. This supervisor supervises:
%% The user-supervisor, the game lobby and the game-supervisor.(AND STATS-SERVER)
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    UserSup = {cc_user_sup, {cc_user_sup, start_link, []},
               permanent, infinity, supervisor, [cc_user_sup]},
    Lobby = {cc_lobby, {cc_lobby, start_link, []},
             permanent, 10000, worker, [cc_lobby]},
    GameSup = {cc_game_sup, {cc_game_sup, start_link, []},
               permanent, infinity, supervisor, [cc_game_sup]},
    {ok, {SupFlags, [UserSup, Lobby, GameSup]}}.

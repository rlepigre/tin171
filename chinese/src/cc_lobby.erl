%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc
%%% The game lobby. Keep track of new and running games and which players are
%%% currently connected.
%%% @end
%%% Created : 28 Mar 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(cc_lobby).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([login/1, leave/0, host_game/1, join_game/1,
         start_game/0, spectate_game/1, list_games/0,
         list_players/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/cc.hrl").

-record(st, {pls = [] :: list(#player{}),
             next_id = 1 :: pos_integer(),
             games = {[],[]} :: tuple(list(#game{}),list(#game{}))
            }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(PlayerName) ->
    gen_server:call(cc_lobby, {login, PlayerName}).

leave() ->
    gen_server:call(cc_lobby, leave).

host_game(GameName) ->
    gen_server:call(cc_lobby, {host_game, GameName}).

join_game(GameName) ->
    gen_server:call(cc_lobby, {join_game, GameName}).

spectate_game(GameName) ->
    gen_server:call(cc_lobby, {spectate, GameName}).

start_game() ->
    gen_server:call(cc_lobby, start_game).

list_games() ->
    gen_server:call(cc_lobby, list_games).

list_players() ->
    gen_server:call(cc_lobby, list_players).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server. No players or games.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #st{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% The lobby receives login, leave, host, join and start messages.
%% @end
%%--------------------------------------------------------------------
handle_call({login, PlayerName}, {PlayerPid,_Ref}, S=#st{pls = Players}) ->
    % Already logged in?
    case find_player(PlayerPid, Players) of
        #player{ppid = PlayerPid} ->
            {reply, error_event(already_logged_in), S};
        false ->
            % Username exists?
            case lists:keymember(PlayerName, #player.name, Players) of
                true -> 
                    {reply, error_event(user_exists), S};
                false  -> 
                    {reply, ok, 
                     S#st{pls = new_player(PlayerName, PlayerPid, Players)}}
            end
    end;
handle_call(leave, {PlayerPid,_Ref}, S=#st{pls = Players, games = Games}) ->
    % Logged in?
    case find_player(PlayerPid, Players) of
        % No? Do nothing.
        false ->
            {reply, ok, S};
        % Idle player, just remove the player
        #player{st = idle} -> 
            {reply, ok, S#st{pls = remove_player(PlayerPid, Players)}};
        % In a game
        Player=#player{st = {in_game, GameName}} ->
            case find_game(GameName, Games) of
                % If game is running, remove player and notify game
                {running, Game} -> 
                    cc_game:leave_game(Game, Player),
                    {reply, ok, S#st{pls = remove_player(PlayerPid, Players)}};
                % It's a unstarted game with one player, remove it.
                {new, Game=#game{num = 1}} -> 
                    {reply, ok, S#st{pls = remove_player(PlayerPid, Players),
                                     games = remove_game(Game, Games)}};
                % It's unstarted and the leaving player is host, close game.
                {new, Game=#game{host = PlayerPid}} ->
                    % Notify players and specs about game closing
                    game_closed(Game),
                    {reply, ok,
                     S#st{pls = remove_player(PlayerPid, 
                                              set_all_idle(Game, Players)),
                          games = remove_game(Game, Games)}};
                % It's unstarted, remove player, notify host
                {new, Game} ->
                    NewGame = remove_player_game(Player, Game),
                    player_left(NewGame),
                    {reply, ok, 
                     S#st{pls = remove_player(PlayerPid, Players),
                          games = update_game(NewGame, Games)}};
                false ->
                    {stop, error_event(game_not_found), S}
            end;
        % Spectating a game
        Player=#player{st = {spectating, GameName}} ->
            case find_game(GameName, Games) of
                % Spectating a running game, tell game about it, remove player
                {running, Game} ->
                    cc_game:leave_game(Game, Player),
                    {reply, ok,   S#st{pls = remove_player(PlayerPid, Players)}};
                   % Spectating a new game, remove player and update games
                {new, Game} ->
                    NewGame = remove_spectator(PlayerPid, Game),
                    {reply, ok,
                     S#st{games = update_game(NewGame, Games)}}
            end
    end;
% GameNames has to be a non-empty list
handle_call({host_game, GameName}, _, S) when length(GameName) =:= 0 ->
    {reply, error_event(name_empty), S};
handle_call({host_game, GameName}, {PlayerPid,_Ref},
            S=#st{next_id = GameId, pls = Players, games = Games}) ->
    case find_player(PlayerPid, Players) of
        % Login first
        false -> {reply, error_event(login_first), S};
        % An idle player
        Player=#player{st = idle} ->
            % Does game already exist?
            case find_game(GameName, Games) of
                false -> NewGame = new_game(Player, GameName, GameId),
                         {reply, ok, 
                          S#st{next_id = GameId + 1,
                               pls = set_in_game(Player, GameName, Players),
                               games = add_game(NewGame, Games)}};
                _ -> {reply, error_event(game_exists), S}
            end;
        % Busy player, playing or spectating
        _Player ->
            {reply, error_event(not_idle), S}
    end;
handle_call({join_game, GameName}, {PlayerPid,_Ref}, 
            S=#st{pls = Players, games = Games}) ->
    case find_player(PlayerPid, Players) of
        false -> {reply, error_event(login_first), S};
        % Idle player, ok!
        Player=#player{st = idle} ->
            case find_game(GameName, Games) of
                % Games doesnt exist
                false -> {reply, error_event(game_doesnt_exist), S};
                % It's a new game with slots
                {new, Game=#game{num = Num}} when Num =< ?MAX_PLAYERS -> 
                    player_joined(Game),
                    {reply, ok, 
                     S#st{pls = set_in_game(Player, GameName, Players),
                          games = add_player(Player, Game, Games)}};
                % It's a full game
                {new, _Game} -> {reply, error_event(game_full), S};
                % It's a running game
                {running, _Game} ->
                    {reply, error_event(game_started), S}
            end;
        % Busy player!
        _Player ->
            {reply, error_event(not_idle), S}
    end;
handle_call({spectate, GameName}, {PlayerPid, _Ref},
            S=#st{pls = Players, games = Games}) ->
    case find_player(PlayerPid, Players) of
        false -> {reply, error_event(login_first), S};
        Player=#player{st = idle} ->
            case find_game(GameName, Games) of
                % Can't spectate an unexisting game!
                false -> {reply, error_event(game_doesnt_exist), S};
                {running, Game} ->
                    cc_game:spectate(Game),
                    {reply, ok, 
                     S#st{pls = set_spectating(Player, GameName, Players)}};
                {new, Game} ->
                    {reply, ok,
                     S#st{pls = set_spectating(Player, GameName, Players), 
                          games = add_spectator(Player, Game, Games)}}
            end;
        _Player ->
            {reply, error_event(already_in_game), S}
    end;
handle_call(start_game, {PlayerPid,_Ref}, S=#st{pls = Players, games = Games}) ->
    case find_player(PlayerPid, Players) of
        false -> {reply, error_event(login_first), S};
        #player{st = {in_game, GameName}} ->
            case find_game(GameName, Games) of
                % Game exists?
                false -> {stop, error_event(couldnt_find_game), S};
                % Can't start a started game
                {running, _Game} ->
                    {reply, error_event(cannot_start_running_game), S};
                % New game, more than one player, start it!
                {new, Game=#game{host = PlayerPid, num = Num}} when Num > 1 ->
                    {reply, ok, S#st{games = start_game(Game, Games)}};
                % Otherwise some error, probably not enough players or not host
                {new, _Game} -> {reply, error_event(cannot_start_game), S}
            end;
        _Player -> {reply, error_event(not_in_game), S}
    end;
handle_call(list_games, _From, S=#st{games = {NGames, RGames}}) ->
    {reply, {games, game_names(NGames), game_names(RGames)}, S};
handle_call(list_players, _From, S=#st{pls = Players}) ->
    {reply, player_names(Players), S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({game_over, GameName}, S=#st{pls = Players, games = Games}) ->
    {running, Game} = find_game(GameName, Games),
    {noreply, S#st{pls = set_all_idle(Game, Players),
                   games = remove_game(Game, Games)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% PLAYER RELATED

% Create new player and add it to the supplied list
new_player(PlayerName, PlayerPid, Players) ->
    P = #player{name = PlayerName, ppid = PlayerPid},
    [P|Players].

% Remove a player from a list of players using the players pid
remove_player(PlayerPid, Players) ->
    lists:keydelete(PlayerPid, #player.ppid, Players).

% Map out the player names
player_names(Players) ->
    lists:map(fun(Player) -> Player#player.name end, Players).

% Find a player using its pid in a list of players
find_player(PlayerPid, Players) ->
    lists:keyfind(PlayerPid, #player.ppid, Players).

% Set player idle in the list of players
set_idle(Player, Players) ->
    NewPlayer = Player#player{st = idle},
    lists:keyreplace(Player#player.ppid, #player.ppid, Players, NewPlayer).

% Set all players in game to be idle in the list of all players
set_all_idle(#game{pls = Players, specs = Specs}, AllPlayers) ->
    lists:foldl(fun(Player, AccPlayers) ->
                        set_idle(Player, AccPlayers) 
                end, AllPlayers, Players ++ Specs).

% Set a player to be in a game
set_in_game(Player, GameName, Players) ->
    NewPlayer = Player#player{st = {in_game, GameName}},
    lists:keyreplace(Player#player.ppid, #player.ppid, Players, NewPlayer).

% Set a player to be spectating
set_spectating(Player, GameName, Players) ->
    NewPlayer = Player#player{st = {spectating, GameName}},
    lists:keyreplace(Player#player.ppid, #player.ppid, Players, NewPlayer).


% GAME STORE

% Create a new game
new_game(Host, GameName, GameId) ->
    #game{name = GameName,
          host = Host#player.ppid,
          id = GameId,
          st = waiting,
          pls = [Host],
          num = 1
         }.

% Update a game in the game store
update_game(NewGame, Games = {NGames, RGames}) ->
    case find_game(NewGame#game.name, Games) of
        {new, _} ->
            NNGames = 
                lists:keyreplace(NewGame#game.name, #game.name, NewGame, NGames),
            {NNGames, RGames};
        {run, _} ->
            NRGames =
                lists:keyreplace(NewGame#game.name, #game.name, NewGame, RGames),
            {NGames, NRGames};
        false ->
            Games
    end.

% Find a game in the game store
% false if it doesnt exist
% {new, Game} if a new game, {running, Game} if its started
find_game(GameName, {NGames,RGames}) ->
    case lists:keyfind(GameName, #game.name, NGames) of
        false ->
            case lists:keyfind(GameName, #game.name, RGames) of
                false ->
                    false;
                Game ->
                    {running, Game}
            end;
        Game ->
            {new, Game}
    end.

% Add a new game to game store
add_game(Game, {NGames,RGames}) ->
    {[Game | NGames], RGames}.

% Map out the game names
game_names(Games) ->
    lists:map(fun(Game) -> Game#game.name end, Games).

% Remove a game from the game store
remove_game(Game, Games={NGames, RGames}) ->
    GameName = Game#game.name,
    case find_game(GameName, Games) of
        false -> Games;
        {new, _Game} ->
            {lists:keydelete(GameName, #game.name, NGames), RGames};
        {running, _Game} ->
            {NGames, lists:keydelete(GameName, #game.name, RGames)}
    end.

% Remove a player from a game
remove_player_game(Player, Game=#game{pls = Players, num = Num}) ->
    Game#game{pls = lists:keydelete(Player#player.ppid, #player.ppid, Players),
              num = Num - 1}.

% Remove a spectator from a game
remove_spectator(PlayerPid, Game=#game{specs = Specs}) ->
    Game#game{specs = remove_player(PlayerPid, Specs)}.

% Start a game, move it from newgames to running games and start it
start_game(Game, {NGames, RGames}) ->
    GameName = Game#game.name,
    {ok, GamePid} = cc_game_sup:start_game(Game),
    {lists:keydelete(GameName, #game.name, NGames),
     [Game#game{gpid = GamePid, st = started}|RGames]}.

% Add a player to a game, update the game store
add_player(Player, Game=#game{pls = Players, num = Num}, {NGames, RGames}) ->
    NewGame = Game#game{pls = Players ++ [Player], num = Num + 1},
    NNGames = lists:keyreplace(Game#game.name, #game.name, NGames, NewGame),
    {NNGames, RGames}.

% Add a spectator to a game, update the game store
add_spectator(Player, Game=#game{specs = Specs}, {NGames, RGames}) ->
    NewPlayer = Player#player{st = {spectating, Game#game.name}},
    NewGame = Game#game{specs = [NewPlayer|Specs]},
    NNGames = lists:keyreplace(Game#game.name, #game.name, NGames, NewGame),
    {NNGames, RGames}.

% COMMUNICATION

% Send Event to Players
send(Event, Players) ->
    lists:foreach(fun(Player) ->
                          gen_server:cast(Player#player.ppid, Event) 
                  end, Players).

% Send Event to Host of Game
send_host(Event, Game) ->
    gen_server:cast(Game#game.host, Event).

% Error event 
error_event(Event) ->
    {error, Event}.

% Player left event
player_left(Game=#game{pls = Players}) ->
    send_host({player_left, player_names(Players)}, Game).

% Game closed
game_closed(Game) -> 
    send(game_closed, Game#game.pls ++ Game#game.specs).

% Player joined
player_joined(Game=#game{pls = Players}) ->
    send_host({player_joined, player_names(Players)}, Game).

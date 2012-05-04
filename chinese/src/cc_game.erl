%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc The game module
%%% This module is the implementation of the game state machine.
%%% It starts in the waiting for players state, then transitions to
%%% the playing state and then ends. Errors may occur and participants
%%% will be notified of all events.
%%% @end
%%% Created : 23 Mar 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(cc_game).

-behaviour(gen_fsm).
-compile(export_all).
-import(lib_cc).
-import(cc_lobby).

-include("include/cc.hrl").

%% API
-export([start_link/1]).
-export([leave_game/2, move/2, spectate/2]).

%% gen_fsm callbacks
-export([init/1, play/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%--------------------------------------------------------------------
start_link(Game) ->
    gen_fsm:start_link(?MODULE, [Game], []).

%%--------------------------------------------------------------------
%% @doc Leave a game
%% @end
%%--------------------------------------------------------------------
leave_game(GamePid, Player) ->
    gen_fsm:sync_send_all_state_event(GamePid, {leave, Player}).

spectate(GamePid, Player) ->
    gen_fsm:sync_send_all_state_event(GamePid, {spectate, Player}).

%%--------------------------------------------------------------------
%% @doc Do a move when it's your turn.
%% @end
%%--------------------------------------------------------------------
move(GamePid, Move) ->
    gen_fsm:sync_send_event(GamePid, {move, Move}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Callback function for gen_fsm. Initializes a game and sets the
%% initial state to wait.
%% @end
%%--------------------------------------------------------------------
init([Game]) ->
    PlayingState = init_state(Game),
    send_game_start(PlayingState),
    {ok, play, PlayingState}.

%%--------------------------------------------------------------------
%% @doc The playing state. The state machine accepts incoming moves from
%% the players. It has to be the player process' turn otherwise an error is
%% sent. The move has to be legal otherwise it is rejected. Players may leave
%% the game in which they are removed, their pegs are removed, if it's their 
%% turn the next player in line is notified and a new host may be appointed.
%% @end
%%--------------------------------------------------------------------
% Is it your turn?
play({move, _Move}, {PlayerPid, _Ref}, S=#game{pls = [Next|_Rest]}) 
  when PlayerPid =/= Next#player.ppid ->
    {reply, error_event(not_your_turn), play, S};
% It's your turn, make a move
play({move, Move}, {_PlayerPid, _Ref}, 
     S=#game{pls = Players, board = Board}) ->
    Player = hd(Players),
    #player{id = PlayerId} = Player,
    {Status, NewBoard} = lib_cc:move(Board, PlayerId, Move),
    BoardStr = lib_cc:board_to_string(NewBoard),
    % Check if move was successful, if it was a winning move or an illegal one
    case Status of
        % Move OK; Send update-message, inform next player, update game state
        ok -> update(Player, Move, BoardStr, S),
              {NextPlayer, NewPlayers} = next_player(Players),
              your_turn(NewBoard, NextPlayer),
              {reply, ok, play, S#game{pls = NewPlayers, board = NewBoard}};
        % Winning move; send won-message, notify lobby and terminate
        won -> notify_lobby(Player, BoardStr, S),
               self() ! stop,
               {reply, ok, play, S};
        % Illegal move; try again!
        illegal_move -> {reply, error_event(illegal_move), play, S}
    end.

%%--------------------------------------------------------------------
%% @doc Handle synchrounos events to all states. Currently only involves
%% a player leaving a game. Leaving a game requires some actions depending
%% on the current state. If a game is empty after someone leaving the game
%% is closed. If the game is in wait-state then a player is removed and everyone
%% is notified. If the game is in play-state a player is removed from the board
%% and the game. All players are notified. If the host leaves a new host is
%% notified
%% @end
%%--------------------------------------------------------------------

%% Spectate a game
handle_sync_event({spectate, Player}, _LobbyPid, play,
                  S=#game{board = Board, pls = Players, specs = Specs}) ->
    spec_game_state(Players, lib_cc:board_to_string(Board), Player),
    {reply, ok, play, S#game{ specs = [Player|Specs] }};
%% Leave a game; not enough players to continue
handle_sync_event({leave, Player}, _LobbyPid, play,
                  S=#game{num = 2, pls = Players, specs = Specs}) ->
    PlayerPid = Player#player.ppid,
    % Is player a player or spectator?
    case cc_lobby:find_player(PlayerPid, Players) of
        false ->
            % Player is a spectator
            case cc_lobby:find_player(PlayerPid, Specs) of
                false -> 
                    {reply, error_event(not_in_game), play, S};
                _P    -> 
                    {reply, ok, play,
                     S#game{specs = cc_lobby:remove_player(PlayerPid, Specs)}}
            end;
        Player ->
            % Remove the player, notify the last that the game is over
            % then notify the lobby and terminate
            [LastPlayer] = cc_lobby:remove_player(Player, Players),
            send(error_event(game_over), LastPlayer),
            %%notify_lobby(S),
            {stop, ok, game_over, S#game{pls = [LastPlayer], num = 1}}
    end;
handle_sync_event({leave, Player}, _LobbyPid, play,
                  S=#game{num = Num, pls = Players, 
                          board = Board, specs = Specs}) ->
    PlayerPid = Player#player.ppid,
    case cc_lobby:find_player(PlayerPid, Players) of
        false ->
            case cc_lobby:find_player(PlayerPid, Specs) of
                false -> {reply, error_event(not_in_game), play, S};
                _P -> 
                    {reply, ok, play,
                     S#game{specs = cc_lobby:remove_player(PlayerPid, Specs)}}
            end;
        #player{id = PlayerId} ->
            NewPlayers = cc_lobby:remove_player(Player, Players),
            Next = hd(Players),
            if Next#player.ppid =:= PlayerPid ->
                    your_turn(Board, hd(NewPlayers))
            end,
            NewBoard = lib_cc:remove_player(Board, PlayerId),
            game_state(NewPlayers, lib_cc:board_to_string(Board), NewPlayers ++ Specs),
            {reply, ok, play, S#game{num = Num - 1, pls = NewPlayers,
                                     board = NewBoard}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminate, delete the board.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #game{board=Board}) ->
    lib_cc:delete_board(Board),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Unused functions
handle_event(Event, StateName,S) ->
    {stop, {not_used, Event, StateName, S}, S}.

handle_info(stop, _StateName, S) ->
    {stop, stopping, S}.

wait(Event, S) ->
    {stop, {not_used, Event, S}, S}.
play(Event, S) ->
    {stop, {not_used, Event, S}, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% INITIALIZATION

%%--------------------------------------------------------------------
%% @doc Initialize the playing state. Create a board, add the players etc.
%% @end
%%--------------------------------------------------------------------
init_state(S=#game{pls = Players, num = Num}) ->
    Board = lib_cc:new_board(),
    PlayerIds = case Num of
                    3 -> [1,6,4];
                    _ -> lists:seq(1,Num)
                end,
    NewPlayers = lists:map(fun({Player, Id}) -> set_id(Player, Id) end,
                           lists:zip(Players, PlayerIds)),
    NewBoard = lists:foldl(fun(Player, BoardAcc) -> 
                                   lib_cc:add_player(BoardAcc,Player#player.id) end, Board, NewPlayers),
    S#game{pls = NewPlayers, board = NewBoard}.

%%--------------------------------------------------------------------
%% @doc Send game start event to all players and the first player to play
%% a your_turn-event.
%% @end
%%--------------------------------------------------------------------
send_game_start(#game{pls = Players, board = Board, specs = Specs}) ->
    BoardStr = lib_cc:board_to_string(Board),
    lists:foreach(fun(Player) ->
                          PlayerId = Player#player.id,
                          send({self(), game_start(PlayerId, Players, BoardStr)}, 
                               Player)
                  end, Players),
    NextPlayer = hd(Players),
    game_state(Players, BoardStr, Specs),
    your_turn(Board, NextPlayer).

%% COMMUNICATION WITH PLAYER

%%--------------------------------------------------------------------
%% @doc Broadcast an event to all players.
%% @end
%%--------------------------------------------------------------------
broadcast(Event, Players) ->
    lists:foreach(fun(P) -> gen_server:cast(P#player.ppid, Event) end, Players),
    ok.

%%--------------------------------------------------------------------
%% @doc Send an event to a player.
%% @end
%%--------------------------------------------------------------------
send(Event, Player) ->
    gen_server:cast(Player#player.ppid, Event).

%%--------------------------------------------------------------------
%% @doc Notify the lobby that the game has started.
%% @end
%%--------------------------------------------------------------------
notify_lobby(Player, BoardStr, Game) ->
    Won = {won, {Player#player.id, Player#player.name}, BoardStr},
    gen_server:cast(cc_lobby, {Won, Game}).

%% EVENTS

%%--------------------------------------------------------------------
%% @doc Your turn-event, with a textual representation of the board.
%% @end
%%--------------------------------------------------------------------
your_turn(Board, Player) ->
    send({your_turn, ?TIMEOUT, lib_cc:board_to_string(Board)}, Player).

%%--------------------------------------------------------------------
%% @doc Game start-event. Send the players given id, all the players names
%% with their corresponding id and the board string.
%% @end
%%--------------------------------------------------------------------
game_start(PlayerId, Players, BoardStr) ->
    {game_start, PlayerId, player_ids(Players), BoardStr}.

game_state(Players, BoardStr, To) ->
    GS = {game_state, player_ids(Players), BoardStr},
    broadcast(GS, To).

spec_game_state(Players, BoardStr, Spectator) ->
    GS = {game_state, player_ids(Players), BoardStr},
    send(GS, Spectator).

%%--------------------------------------------------------------------
%% @doc Won-event. A player won the game, the player name and the winning
%% board is sent along.
%% @end
%%--------------------------------------------------------------------
won(Player, BoardStr, #game{pls = Players, specs = Specs}) ->
    Won = {won, {Player#player.id, Player#player.name}, BoardStr},
    broadcast(Won, Players ++ Specs).

%%--------------------------------------------------------------------
%% @doc Update-event. Sent when someone made a move
%% @end
%%--------------------------------------------------------------------
update(Player, Move, BoardStr, #game{pls = Players, specs = Specs}) ->
    Update = {update, {Player#player.id, Player#player.name}, list_to_tuple(Move), BoardStr},
    broadcast(Update, Players ++ Specs).

%%--------------------------------------------------------------------
%% @doc Error-event. Sent when something is wrong.
%% @end
%%--------------------------------------------------------------------
error_event(Reason) ->
    {error, Reason}.

%% PLAYER RELATED

%%--------------------------------------------------------------------
%% @doc Set the identifier of the player
%% @end
%%--------------------------------------------------------------------
set_id(Player, Id) ->
    Player#player{id = Id}.                    

%%--------------------------------------------------------------------
%% @doc Return a list of tuples with the player names and ids 
%% @end
%%--------------------------------------------------------------------
player_ids(Players) ->
    lists:map(fun(P) ->
                      {P#player.id, P#player.name}
              end, Players).

%%--------------------------------------------------------------------
%% @doc Rotate the player list.
%% @end
%%--------------------------------------------------------------------
next_player([Prev|Rest]) ->
    NewPlayers = Rest ++ [Prev],
    Next = hd(NewPlayers),
    {Next, NewPlayers}.

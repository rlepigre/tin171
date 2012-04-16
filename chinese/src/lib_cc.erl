%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc
%%% Library module for the chinese checkers game.
%%% Contains functions to create, manipulate and observe a game board.
%%% @end
%%% Created : 10 Apr 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(lib_cc).

-include_file("cc.hrl").

%% API
-export([new_board/0, delete_board/1, add_player/2, remove_player/2, move/3, 
         board_to_string/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create a new game board. The game board is a digraph with each
%% edge beeing a possible move. 
%% @end
%%--------------------------------------------------------------------
new_board() ->
    Arr = array:new([{size,288},{default,off_board}]),
    lists:foldl(fun(N, Acc) -> array:set(N,0, Acc) 
                end, Arr, whole_board()).

delete_board(_Board) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Add a player to the board. Set all the pegs of that player in
%% their nest.
%% @end
%%--------------------------------------------------------------------
add_player(Board,Player) ->
    lists:foldl(fun(N, Arr) -> array:set(N,Player, Arr) end,
                Board,nest(Player)).

%%--------------------------------------------------------------------
%% @doc Remove a player from the board.
%% @end
%%--------------------------------------------------------------------
remove_player(Board, Player) ->
    array:map(fun(off_board) -> 
                      off_board;
                 (Peg) when Peg =:= Player ->
                      0;
                 (OtherPlayer) ->
                      OtherPlayer
              end, Board).

%%--------------------------------------------------------------------
%% @doc Move a peg. A move can be a single step or a series of jumps.
%% If the move is accepted then ok is returned, else illegal move.
%% @end
%%--------------------------------------------------------------------
move(Board, Player, Moves = [Start|_Rest]) when length(Moves) > 1 ->
    StartStatus = check_start(Board, Player, Start),
    Stop = lists:last(Moves),
    StopStatus = check_stop(Board, Player, Stop),
    AllOnBoard = all_on_board(Moves),
    if StartStatus, StopStatus, AllOnBoard, Start =/= Stop ->
            case legal_move(Board,Moves) of
                true ->
                    NewBoard = do_move(Board, Moves),
                    case check_win(NewBoard, Player) of
                        true  -> {won, NewBoard};
                        false -> {ok, NewBoard}
                    end;
                false ->
                    {illegal_move, Board}
            end;
       true ->
            {illegal_move, Board}
    end;
move(_, _, _) ->
    illegal_move.

%%--------------------------------------------------------------------
%% @doc Turn a board into a textual representation as a uni-dimensional
%% vector of characters. Each position can be: 
%% "#" : A undefined position 
%% " " : An empty hole
%% "N" : A player number (N = 1..6)
%% @end
%%--------------------------------------------------------------------
board_to_string(Board) ->
    lists:map(fun peg_to_char/1, array:to_list(Board)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Return the positions of all the pegs in a players nest.
%% @end
%%--------------------------------------------------------------------
nest(1) ->
    [4,21,22,38,39,40,55,56,57,58];
nest(2) ->
    [230,231,232,233,248,249,250,266,267,284];
nest(3) ->
    [68,69,70,71,86,87,88,104,105,122];
nest(4) ->
    [166,183,184,200,201,202,217,218,219,220];
nest(5) ->
    [77,78,79,80,95,96,97,113,114,131];
nest(6) ->
    [157,174,175,191,192,193,208,209,210,211].

%%--------------------------------------------------------------------
%% @doc Returns the positions of the pegs in the middle of the board.
%% @end
%%--------------------------------------------------------------------
middle() ->
    lists:seq(72,76) ++ lists:seq(89,94) ++
        lists:seq(106,112) ++ lists:seq(123,130) ++
        lists:seq(140,148) ++ lists:seq(158,165) ++
        lists:seq(176,182) ++ lists:seq(194,199) ++
        lists:seq(212,216).

%%--------------------------------------------------------------------
%% @doc Returns the positions of all pegs on the board.
%% @end
%%--------------------------------------------------------------------
whole_board() ->
    lists:concat(lists:map(fun(N) -> nest(N) end,
                           lists:seq(1,6))) ++ middle().

on_board(Peg) ->
    lists:member(Peg, whole_board()).

all_on_board(Moves) ->
    lists:all(fun on_board/1, Moves).

%%--------------------------------------------------------------------
%% @doc Turn a peg position and a board into a character.
%% @end
%%--------------------------------------------------------------------
peg_to_char(off_board) ->
    $#;
peg_to_char(0) ->
    $ ;
peg_to_char(N) ->
    N+48.

%%--------------------------------------------------------------------
%% @doc Check if a player has won.
%% @end
%%--------------------------------------------------------------------
check_win(Board, Player) ->
    GoalNest = goal_nest(Player),
    all_pegs(Board, Player, GoalNest).

%%--------------------------------------------------------------------
%% @doc Check if all of Player's pegs are in GoalNest
%% @end
%%--------------------------------------------------------------------
all_pegs(Board, Player, GoalNest) ->
    lists:all(fun(N) ->
                      array:get(N,Board) =:= Player end,
              nest(GoalNest)).

%%--------------------------------------------------------------------
%% @doc Return the goal nest of a player
%% @end
%%--------------------------------------------------------------------
goal_nest(Player) ->
    case lists:member(Player, [1,3,5]) of
        true  -> Player+1;
        false -> Player-1
    end.        

%%--------------------------------------------------------------------
%% @doc Move a peg from the head of Moves to the last of Moves
%% @end
%%--------------------------------------------------------------------
do_move(Board, Moves) ->
    move_peg(Board, hd(Moves), lists:last(Moves)).

%%--------------------------------------------------------------------
%% @doc Move a peg.
%% @end
%%--------------------------------------------------------------------    
move_peg(Board, Start, Stop) ->
    Player = array:get(Start, Board),
    Arr = array:set(Start, 0, Board),
    array:set(Stop, Player, Arr).

%%--------------------------------------------------------------------
%% @doc Check if a player occupies a peg.
%% @end
%%--------------------------------------------------------------------
check_start(Board, Player, Peg) ->
    get_peg(Board, Peg) =:= Player.

%%--------------------------------------------------------------------
%% @doc Check if a hole is empty.
%% @end
%%--------------------------------------------------------------------
check_stop(Board, Player, Peg) ->
    get_peg(Board, Peg) =:= 0 andalso
        lists:member(Peg, possible_positions(Player)).

%%--------------------------------------------------------------------
%% @doc Check if a move is legal.
%% @end
%%--------------------------------------------------------------------
legal_move(Board, Moves) ->
    case is_step(Moves) of
        true -> legal_step(Moves);
        false -> legal_jump(Board, Moves)
    end.

%%--------------------------------------------------------------------
%% @doc Check if a step is legal.
%% @end
%%--------------------------------------------------------------------    
legal_step([Start,Stop]) ->
    lists:member(Stop, allowed_moves(Start));
legal_step(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Check if a jump is legal.
%% @end
%%--------------------------------------------------------------------
legal_jump(Board, [First,Second|Rest]) ->
    JS = lists:zip(allowed_moves(First), allowed_jumps(First)),
    case lists:keyfind(Second, 2, JS) of
        false ->
            false;
        {Step, Second} ->
            case get_peg(Board, Step) of
                off_board ->
                    false;
                0 ->
                    false;
                _N ->
                    if length(Rest) > 0 ->
                            legal_jump(Board, [Second|Rest]);
                        true -> 
                            true
                    end
            end
    end. 

%%--------------------------------------------------------------------
%% @doc Return the indices of all pegs on board that belong to a player
%% @end
%%--------------------------------------------------------------------
get_player_ix(Board, Player) ->
    lists:foldl(fun({Ix, Peg}, PegAcc) when Peg =:= Player ->
                        [Ix|PegAcc];
                   (_, PegAcc) ->
                        PegAcc
                end, lists:zip(lists:seq(0,288), array:to_list(Board))).

%%--------------------------------------------------------------------
%% @doc Return all legal jumps that can be made by player on the board
%% @end
%%--------------------------------------------------------------------
legal_jumps(Board, Player) ->
    Pegs = get_player_ix(Board, Player),
    lists:subtract(legal_jumps(Board, Pegs, []), Pegs).

legal_jumps(_Board, [], Jumps) ->
    Jumps;
legal_jumps(Board, PossibleJumps, Jumps) ->
    NewPossible = 
        lists:map(
          fun(PossibleJump) ->
                  JS = lists:zip(allowed_moves(PossibleJump),
                                 allowed_jumps(PossibleJump)),
                  lists:filter(fun({S, J}) -> 
                                       case get_peg(Board, J) of
                                           off_board ->
                                               false;
                                           0 ->
                                               case get_peg(Board,S) of
                                                   0 -> false;
                                                   _ -> true
                                               end;
                                           _N ->
                                               false
                                       end
                               end, JS) end, PossibleJumps),
    NewJumps = PossibleJumps ++ Jumps,
    NewPossible2 = lists:subtract(NewPossible, NewJumps),
    legal_jumps(Board, NewPossible2, NewJumps).

%%--------------------------------------------------------------------
%% @doc Check if a moves is a allowed step
%% @end
%%--------------------------------------------------------------------
is_step([Start,Stop]) ->
    lists:member(Stop,allowed_moves(Start));
is_step(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Return the allowed moves of a peg
%% @end
%%--------------------------------------------------------------------
allowed_moves(Pos) ->
    [Pos-18, Pos-17, Pos-1, Pos+1, Pos+17, Pos+18].

%%--------------------------------------------------------------------
%% @doc Return the allowed jumps of a peg
%% @end
%%--------------------------------------------------------------------
allowed_jumps(Pos) ->
    [Pos-36, Pos-34, Pos-2, Pos+2, Pos+34, Pos+36].

%%--------------------------------------------------------------------
%% @doc Return all the allowed positions for a players peg
%% @end
%%--------------------------------------------------------------------
possible_positions(Player) ->
    nest(goal_nest(Player)) ++ nest(Player) ++ middle().    

%%--------------------------------------------------------------------
%% @doc Return the state of the peg at given position
%% @end
%%--------------------------------------------------------------------
get_peg(Board, Pos) ->
    array:get(Pos, Board).

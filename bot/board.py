# -*- coding: utf-8 -*-

# Copyright (C) 2012  Salvo "LtWorf" Tomaselli
# Copyright (C) 2012 Göran Weinholt
#
# This is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# author Salvo "LtWorf" Tomaselli <tiposchi@tiscali.it>
# author Göran Weinholt

BOARD_LENGTH = 17*17
PEG_IDS = "x123456"
INVALID = '#'
EMPTY = ' '
NEIGHBORS = [ +1, -1, -18, -17, +18, +17 ]
FULL_BOARD = '####1################11###############111##############1111#########3333     5555#####333      555######33       55#######3        5########         ########6        4#######66       44######666      444#####6666     4444#########2222##############222###############22################2###'

def is_on_board(n):
    """True if position n is inside the board."""
    return n >= 0 and n < BOARD_LENGTH

def is_occupied(board, n):
    """True if position n a valid but occupied position."""
    return board[n] not in (INVALID, EMPTY)

def is_empty(board, n):
    """True if position n is a valid empty position."""
    return board[n] == EMPTY

def is_valid_stop(n, peg_id):
    """True if peg_id is allowed to stop at position n."""
    ## TODO: should this use the initial board that the server created?
    return FULL_BOARD[n] in (PEG_IDS[peg_id], EMPTY)

def peg_positions(board, peg_id):
    """The positions of all pegs with the given id."""
    return [ x for x in range(len(board))
             if board[x] == PEG_IDS[peg_id] ]

def adjacent(n):
    """The board positions that are adjacent to n. It is legal to move
    from n to one of these positions if that positions is
    unoccupied."""
    return [ (n-x) for x in NEIGHBORS if is_on_board(n-x) ]

def jumps(n):
    """The board positions that can be jumped to from n. Returns
    tuples of (dest,chk). It is legal to move to a dest position if
    the chk position is occupied by another peg."""
    return [ ((n-x*2), n-x) for x in NEIGHBORS
             if is_on_board(n-x) and is_on_board(n-x*2) ]

def available_moves(board, initial, first=True, jumped=[]):
    """Generates all moves for a given peg that starts at position
    'initial'."""
    # First try to move without jumping (unless already jumping)
    if first:
        for i in adjacent(initial):
            if is_empty(board, i) and not i in jumped:
                yield jumped + [initial, i]
    # Now try jumping.
    path = jumped + [initial]
    for dest, chk in jumps(initial):
        if is_empty(board, dest) and is_occupied(board, chk) and \
                not dest in jumped:
            yield path + [dest]
            for j in available_moves(board, dest, False, path):
                yield j

def all_moves(board, peg_id):
    """All legal moves for all the player's pegs."""
    for i in peg_positions(board, peg_id):
        for move in available_moves(board, i):
            if is_valid_stop(move[-1], peg_id):
                yield move

def run_tests():
    board = '####1################11###############111##############1111#########             #####            ######           #######          ########         ########          #######           ######            #####             #########2222##############222###############22################2###'
    ma1 = list(all_moves(board, 1))
    ma2 = list(all_moves(board, 2))
    if len(ma1) != len(ma2):
        print "Players have different numbers of legal moves"
        print ma1, ma2
    if len([x for x in ma1 if len(x) > 2]) != 0:
        print 'Player 1 should at most be able to jump once'
        print ma1
    # Player 1 makes a jump
    board = '####1################11###############11 ##############1111#########        1    #####            ######           #######          ########         ########          #######           ######            #####             #########2222##############222###############22################2###'
    mb1 = list(all_moves(board, 1))
    mb2 = list(all_moves(board, 2))
    if len(ma2) != len(mb2):
        print 'Player 2 suddenly has more legal moves'
        print ma2, mb2
    if len(ma1) >= len(mb1):
        print "Player 1 did not get more moves"
        print ma1, mb1
    if len([x for x in mb1 if len(x) > 2]) == 0:
        print "Player 1 did not get any three-step jumps"
        print mb1
    board = '#### ################11############### 11##############1111#########    1 1      #####            ######           #######          ########         ########          #######           ######            #####    22       #########2222##############2  ###############22################2###'
    mc1 = list(all_moves(board, 1))
    print mc1
    print "ok"


if __name__ == "__main__":
    run_tests()

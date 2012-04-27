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

from math import sqrt

BOARD_LENGTH = 17*17
PEG_IDS = "x123456"
INVALID = '#'
EMPTY = ' '
NEIGHBORS = [ +1, -1, -18, -17, +18, +17 ]
FULL_BOARD = '####1################11###############111##############1111#########3333     5555#####333      555######33       55#######3        5########         ########6        4#######66       44######666      444#####6666     4444#########2222##############222###############22################2###'
REVERSED_FULL_BOARD = {' ': [72, 73, 74, 75, 76, 89, 90, 91, 92, 93, 94, 106, 107, 108, 109, 110, 111, 112, 123, 124, 125, 126, 127, 128, 129, 130, 140, 141, 142, 143, 144, 145, 146, 147, 148, 158, 159, 160, 161, 162, 163, 164, 165, 176, 177, 178, 179, 180, 181, 182, 194, 195, 196, 197, 198, 199, 212, 213, 214, 215, 216], '#': [0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 59, 60, 61, 62, 63, 64, 65, 66, 67, 81, 82, 83, 84, 85, 98, 99, 100, 101, 102, 103, 115, 116, 117, 118, 119, 120, 121, 132, 133, 134, 135, 136, 137, 138, 139, 149, 150, 151, 152, 153, 154, 155, 156, 167, 168, 169, 170, 171, 172, 173, 185, 186, 187, 188, 189, 190, 203, 204, 205, 206, 207, 221, 222, 223, 224, 225, 226, 227, 228, 229, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 285, 286, 287], '1': [4, 21, 22, 38, 39, 40, 55, 56, 57, 58], '3': [68, 69, 70, 71, 86, 87, 88, 104, 105, 122], '2': [230, 231, 232, 233, 248, 249, 250, 266, 267, 284], '5': [77, 78, 79, 80, 95, 96, 97, 113, 114, 131], '4': [166, 183, 184, 200, 201, 202, 217, 218, 219, 220], '6': [157, 174, 175, 191, 192, 193, 208, 209, 210, 211]}
OPPOSITES=[None,2,1,4,3,6,5]

def pretty_print(board):
    for i in range(len(board)):
        if i %17==0: print
        print board[i],
        

def static_distance_from_target(board,player):
    
    data=(None,
          (-1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, 13, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, 12, 12, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 11, 11, 11, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 10, 10, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 10, 9, 9, 9, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 9, 9, 8, 8, 9, 9, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 8, 8, 7, 7, 7, 8, 8, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 7, 6, 6, 6, 6, 7, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 6, 5, 5, 5, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, 4, 4, 4, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 3, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1),
          (-1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 2, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 3, 3, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 4, 4, 4, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, 4, 4, 4, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 6, 5, 5, 5, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 7, 6, 6, 6, 6, 7, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 8, 8, 7, 7, 7, 8, 8, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 9, 9, 8, 8, 9, 9, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 10, 9, 9, 9, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 10, 10, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 11, 11, 11, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, 12, 12, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, 13, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 15, -1, -1, -1, -1),
          (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 15, 14, 13, 12, 11, 11, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, 13, 12, 11, 10, 10, 9, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, 12, 11, 10, 9, 9, 8, 7, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, 11, 10, 9, 8, 7, 6, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 9, 8, 7, 6, 5, 4, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 9, 7, 6, 5, 4, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 9, 8, 6, 5, 4, 4, 3, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 8, 7, 6, 4, 4, 3, 2, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 7, 7, 5, 4, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
          (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 7, 7, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 2, 3, 4, 4, 6, 7, 8, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, 3, 4, 4, 5, 6, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 4, 4, 5, 6, 7, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 4, 5, 6, 7, 8, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, 6, 6, 7, 8, 9, 10, 11, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 7, 8, 9, 9, 10, 11, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 8, 9, 10, 10, 11, 12, 13, 14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 10, 10, 11, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
          (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 10, 10, 11, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, 7, 8, 9, 10, 10, 11, 12, 13, 14, -1, -1, -1, -1, -1, -1, -1, -1, 7, 7, 8, 9, 9, 10, 11, 12, 13, -1, -1, -1, -1, -1, -1, -1, -1, 5, 6, 6, 7, 8, 9, 10, 11, 12, -1, -1, -1, -1, -1, -1, -1, -1, 4, 4, 5, 6, 7, 8, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, 3, 4, 4, 5, 6, 7, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1, -1, 2, 3, 4, 4, 5, 6, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1, -1, 1, 2, 3, 4, 4, 6, 7, 8, 10, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 7, 7, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
          (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 7, 7, 5, 4, 3, 2, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, 10, 8, 7, 6, 4, 4, 3, 2, 1, -1, -1, -1, -1, -1, -1, -1, -1, 11, 9, 8, 6, 5, 4, 4, 3, 2, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 9, 7, 6, 5, 4, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, 11, 10, 9, 8, 7, 6, 5, 4, 4, -1, -1, -1, -1, -1, -1, -1, -1, 12, 11, 10, 9, 8, 7, 6, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, 13, 12, 11, 10, 9, 9, 8, 7, 7, -1, -1, -1, -1, -1, -1, -1, -1, 14, 13, 12, 11, 10, 10, 9, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, 15, 14, 13, 12, 11, 11, 10, 10, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1))
    pos = [i for i in xrange(288) if board[i]==str(player)]
    distances = [data[player][i] for i in pos]
    return reduce ((lambda a,b:a+b),distances,0)
    
def euclidean_distance(a,b):
    '''Distance between two points on the board, not sure if this is the most
    effective distance function possible, but it should work.
    
    It respects the 3 properties of a distance function
    '''
    coord = lambda p : ((p-p%17)/17, p%17)
    dist= lambda c1,c2: sqrt(((c1[0]-c2[0])**2) + ((c1[1]-c2[1])**2))
    
    a=coord(a)
    b=coord(b)
    
    return dist (a,b)

def euclidean_distance_from_target(board,player,target=None):
    '''Returns the total distance of all the marbles to the target.
    Marbles on the target return 0
    
    Board is a linear array representing the board
    Player is an int indicating the id of the player, it can be an int or a
    string of 1 byte
    
    Target is the id of the target positions (refer to REVERSED_FULL_BOARD
    to better understand that).
    It can be an int or a string of 1 byte
    '''
    if target==None:
        target=OPPOSITES[int(player)]
    
    target = list(REVERSED_FULL_BOARD[str(target)])
    marbles=[]
    
    for i in xrange(len(board)):
        if board[i] == str(player):
            if i not in target:
                marbles.append(i)
            else: 
                target.remove(i)
    dist=lambda a,b: euclidean_distance(a,b)#**2
    return reduce ((lambda a,b:a+b),map(dist,marbles,target),0.0)
    
    pass

def update_board(board, move):
    l = list(board)
    tmp = l[move[0]]
    l[move[0]] = EMPTY
    l[move[-1]] = tmp
    return ''.join(l)

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
    return FULL_BOARD[n] in (PEG_IDS[peg_id], EMPTY,
                             PEG_IDS[OPPOSITES[peg_id]])

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

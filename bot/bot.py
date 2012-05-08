#!/usr/bin/env python
# -*- coding: utf-8 -*-
#TODO Nice pypy arguments --jit threshold=3
# Chinese checkers bot
# Copyright (C) 2012 Göran Weinholt

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import random
import sys
import time

import protocol
from protocol import A
from board import *
import parbot

def evolved_distance_bot(c,timeout,board,player_id):
    return trivial_bot(c,timeout,board,player_id,evolved_distance)

def static_distance_bot(c,timeout,board,player_id):
    return trivial_bot(c,timeout,board,player_id,static_distance_from_target)

def trivial_bot(c, timeout, board, player_id,distance_function=euclidean_distance_from_target):
    key=lambda x:distance_function(update_board(board, x), player_id)
    moves = list(all_moves(board, player_id))
    random.shuffle(moves) # In this way the bots don't get stuck.
    moves.sort(key=key)
    print "moves", moves
    c.move(moves[0])

def iddfs_bot(c, timeout, board, player_id,distance_function=euclidean_distance_from_target):
    def board_evaluation(board, player_id):
        # The bot tries to minimize the distance.
        return -distance_function(board, player_id)

    def recursive_dls(first_move, board, limit, depth, best, stoptime):
        # Depth-limited search.
        score = board_evaluation(board, player_id)
        if score > best[1] or (score == best[1] and depth < best[2]):
            # Maximize the board evaluation function. If a move is
            # just as good as another move, but it arrives at the
            # better outcome sooner (lower depth), it is given
            # preference.
            print "The move", (first_move, score, depth), "is better than", best
            best[0] = first_move
            best[1] = score
            best[2] = depth

        if (time.time() >= stoptime and best[0] != []) or limit <= 0:
            return

        for move in all_moves(board, player_id):
            nboard = update_board(board, move)
            recursive_dls(first_move, nboard, limit - 1, depth + 1, best, stoptime)

    best = [[], -2**24, 2**24]
    stoptime = time.time() + timeout / 1000.0


    # Iterative deepening.
    for depth in xrange(1, 2**24):
        for move in all_moves(board, player_id):
            print "# Considering move", move, "at depth limit", depth
            recursive_dls(move, update_board(board, move), depth, 0, best, stoptime)
        if time.time() >= stoptime and best[0] != []:
            break

    print "best", best
    c.move(best[0])

def evolved_iddfs_bot(c, timeout, board, player_id):
    return iddfs_bot(c, timeout, board, player_id, evolved_distance)

def static_iddfs_bot(c, timeout, board, player_id):
    return iddfs_bot(c, timeout, board, player_id, static_distance_from_target)

def minimax_bot(c, timeout, board, player_id,distance_function=static_distance_from_target):
    def board_evaluation(board, pid):
        return distance_function(board, pid)

    def minimax(board, pid, depth):
        opp = (pid % 2) + 1
        # Player won? Very high score.
        if won(board, pid, opp):
            ##print "Winning move found!"
            return -(2**24)
        # Depth reached
        elif depth == 0:
            return board_evaluation(board, pid)
        # Else branch on all moves
        else:
            best = None
            for move in all_moves(board, opp):
                val = -minimax(update_board(board, move), opp, depth-1)
                if best == None or val > best:
                    best = val
            return best
    
    depth = 2
    best = None
    score = -(2**24)
    for move in all_moves(board, player_id):
        val = -minimax(update_board(board, move), player_id, depth)
        print "Considering move: ", move, " with score: ", val
        if best == None or val > score:
            print "Move: ", move, " was better than ", best
            best = move
            score = val
    print "Choosed: ", best
    c.move(best)

'''
def alphabeta_bot(c, timeout, board, player_id, distance_function=static_distance_from_target):

    def alphabeta(b, d, 

    depth = 2
    best_val, best_move = None
    for move in all_moves(board, player_id):
        if best_val is not None:
            opp_beta = -1 * best_val
        else:
            opp_beta = None
        val = -1 * alphabeta_value(update_board(board, move), depth, None, opp_beta)
        if best_val is None or val > best_val:
            (best_val, best_move) = (val, move)
    print "Choosed: ", best_move
    c.move(best_move)
'''

def play(c, player_id,make_move):
    """Play until someone wins... or something goes wrong."""
    while True:
        x = c.read_noerror()
        if x[0] == A('your_turn'):
            (_, timeout, board) = x
            make_move(c, timeout, board, player_id)
        elif x[0] == A('won'):
            print "A winner was announced:", x
            return

def list_personalities(*stuff):
    '''Prints a list of strings with all the available personalities for the
    bot'''
    r=[]
    for i in personality:
        r.append(i.func_name)

    for i in xrange(len(r)):
        print '%d\t\t%s' % (i,r[i])
    sys.exit(0)
    return r

def main():
    import socket, optparse

    parser = optparse.OptionParser('usage: %prog [options]')
    parser.add_option('-s','--server',help='Server name',
                      action='store',
                      dest='server', default='localhost')
    parser.add_option('-p','--port',help='Port number',
                      action='store',
                      dest='port', default=8000)
    parser.add_option('-n','--nick',help='Client nick name',
                      action='store',
                      dest='nick', default=None)
    parser.add_option('-b','--bot',default=0,help='Personality id',
                      action='store',dest='bot',type='int')
    parser.add_option('-l','--list-personalities',help='lists all the available personalities for the bot',
                      action='callback',callback=list_personalities)
    parser.add_option('-g','--game',help='name of the game to join',action='store',
                      dest='game',default=None)
    parser.add_option('-H','--host',help='name of the game to host',action='store',
                      dest='host',default=None)
    parser.add_option('-y','--players',help='number of players to in the match before starting the game. Ignored when not hosting a game',
                      type='int',action='store',dest='players',default=2)
    (opts, args) = parser.parse_args()


    c = protocol.Client(socket.create_connection((opts.server, opts.port)).makefile())

    if opts.game!=None and opts.host!=None:
        print "can't host and join at the same time"
        sys.exit(0)

    if opts.nick==None:
        opts.nick='Bot-%s-%d'% (personality[opts.bot].func_name,random.randint(100,999))


    c.do_login(opts.nick)
    print "I am", opts.nick

    if opts.host != None:
        c.host_game(opts.host)

        count=opts.players-1

        while count>0:
            x = c.read_noerror()
            if x[0] == A('player_joined'):
                count-=1
        c.start_game()

    else:
        (new, running) = c.list_games()
        if new:
            if opts.game == None:
                game = new[0]
            else:
                game = opts.game
            print "Joining game", game
            c.join_game(game)
        else:
            game = 'Game-'+str(random.randint(100,999))
            print "There are no new games. Hosting game",game,"and waiting for a player"
            c.host_game(game)
            (msg, players) = c.read_noerror()
            if msg == A('player_joined'):
                c.start_game()
                print "Starting game with players:", players
            else:
                raise Exception("I don't know what happened")


    while True:
        x = c.read_noerror()
        if x[0] == A('game_start'):
            print "The game starts."
            (_, player_id, players, board) = x
            play(c, player_id,personality[opts.bot])
            return




personality = (trivial_bot,
               static_distance_bot,
               iddfs_bot,
               static_iddfs_bot, 
               minimax_bot,
               parbot.parallel_static_bot,
               evolved_distance_bot, 
               evolved_iddfs_bot,
               parbot.parallel_euclidean_bot,
               parbot.parallel_evolved_bot,
               )

if __name__ == "__main__":
    main()

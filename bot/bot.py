#!/usr/bin/env python
# -*- coding: utf-8 -*-
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

import protocol
from protocol import A
from board import *


def static_distance_bot(c,timeout,board,player_id):
    return trivial_bot(c,timeout,board,player_id,static_distance_from_target)

def trivial_bot(c, timeout, board, peg_id,distance_function=euclidean_distance_from_target):
    key=lambda x:distance_function(update_board(board, x), peg_id)
    # Just pick the longest move sequence. :)
    moves = list(all_moves(board, peg_id))
    moves.sort(key=key)
    #moves.sort(key=len, reverse=True)
    print "moves", moves
    c.move(moves[0])

    
def play(c, peg_id,make_move):
    """Play until someone wins... or something goes wrong."""
    while True:
        x = c.read_noerror()
        if x[0] == A('your_turn'):
            (_, timeout, board) = x
            make_move(c, timeout, board, peg_id)
        elif x[0] == A('won'):
            print "A winner was announced:", x
            return

def list_personalities(*stuff):
    '''Prints a list of strings with all the available personalities for the
    bot'''
    import sys
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
    (opts, args) = parser.parse_args()
    
    
    c = protocol.Client(socket.create_connection((opts.server, opts.port)).makefile())
    
    if (opts.game==None and opts.host!=None) or (opts.game!=None and opts.host==None):
        print "can't host and join at the same time"
        sys.exit(0)
    
    if opts.nick==None:
        opts.nick='Bot-%s-%d'% (personality[opts.bot].func_name,random.randint(100,999))
    
    
    c.do_login(opts.nick)
    print "I am", opts.nick
    
    if opts.host != None:
        c.host_game(opts.host)
    
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
            raise Exception("I don't know what happened", x)
    while True:
        x = c.read_noerror()
        if x[0] == A('game_start'):
            print "The game starts."
            (_, peg_id, players, board) = x
            play(c, peg_id,personality[opts.bot])
            return



    
personality = (trivial_bot,static_distance_bot)

if __name__ == "__main__":
    main()

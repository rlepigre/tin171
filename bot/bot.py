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

def make_move(c, timeout, board):
    ## TODO: merely write the bot
    c.leave()

def play(c):
    """Play until someone wins... or something goes wrong."""
    while True:
        x = c.read_noerror()
        if x[0] == A('your_turn'):
            (_, timeout, board) = x
            make_move(c, timeout, board)
        elif x[0] == A('won'):
            print "A winner was announced:", x
            return

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
                      dest='nick', default='Bot-'+str(random.randint(100,999)))
    (opts, args) = parser.parse_args()

    c = protocol.Client(socket.create_connection((opts.server, opts.port)).makefile())
    c.do_login(opts.nick)
    print "I am", opts.nick
    (new, running) = c.list_games()
    if new:
        game = new[0]
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
            play(c)
            return


if __name__ == "__main__":
    main()

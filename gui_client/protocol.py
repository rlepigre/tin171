# -*- coding: utf-8 -*-

# Copyright (C) 2012  Salvo "LtWorf" Tomaselli
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

def login_message():
    import os
    import socket

    username = os.getlogin() + "@" + socket.gethostname()
    
    #TODO username could be something else
    
    return "{login,\"%s\"}.\n" % username

def host_game_message(name):
    return "{host_game,\"%s\"}.\n" % name
    
def spectate_game(name):
    return "{spectate,\"%s\"}.\n" % name
    
def join_game(name):
    return "{join_game,\"%s\"}.\n" % name

def start_game():
    return "start_game.\n"
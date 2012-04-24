#!/usr/bin/env python
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

import protocol
from protocol import A

import subprocess
import random

def run_match(server,port,path,bots):
    '''Runs a match between bots
    server:     server's hostname
    port:       server's port
    path:       bot's path
    bots:       list of bot id
    
    This function should collect some stats on the game
    '''
    
    game_name = 'automated-match-%u' % random.randint(0,7000000)
    
    gladiators=[]

    #Launches 1st bot and creates the game
    host_gladiator = subprocess.Popen([path,'-s',server,'-p',str(port),'-H',game_name,'-y',str(len(bots)),'-b',str(bots[0])])
    gladiators.append(host_gladiator)
    
    #TODO spectate match here
    import time
    time.sleep(30)
    
    for i in bots[1:]:
        proc=subprocess.Popen([path,'-s',server,'-p',str(port),'-g',game_name,'-b',str(i)])
        gladiators.append(proc)
    
    #TODO collect all updates and won here
    
    for i in gladiators:
        i.wait()
    
    pass


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
import os
import socket

def run_match(server,port,path,bots,max_updates=1000):
    '''Runs a match between bots
    server:     server's hostname
    port:       server's port
    path:       bot's path
    bots:       list of bot id
    
    This function should collect some stats on the game
    '''
    results= {}
    results['boards']=[]
    
    c = protocol.Client(socket.create_connection((server,port)).makefile())
    
    game_name = 'automated-match-%u' % random.randint(0,7000000)
    
    logdir='/tmp/%s' % game_name
    os.mkdir(logdir)
    print logdir
    
    
    gladiators=[]

    #Launches 1st bot and creates the game
    logfile=open('%s/Log-%s'%(logdir,str(bots[0])),'w')
    host_gladiator = subprocess.Popen([path,'-s',server,'-p',str(port),'-H',game_name,'-y',str(len(bots)),'-b',str(bots[0])],
                                      stdout=logfile,stderr=logfile)
    gladiators.append(host_gladiator)

    #Spectate the game
    for count in xrange(60):
        if count>50:
            print "Game can't be spectated"
            gladiators[0].kill()
            sys.exit(1)
        x=c.list_games()
        if game_name in x[0]:
            break
    c.do_login("watcher-%s" % game_name)
    c.spectate(game_name)
    
    #Launch all the other bots
    for i in bots[1:]:
        logfile=open('%s/Log-%s'%(logdir,str(i)),'w')
        proc=subprocess.Popen([path,'-s',server,'-p',str(port),'-g',game_name,'-b',str(i)],
                              stdout=logfile,stderr=logfile)
        gladiators.append(proc)
    
    
    for count in xrange(max_updates):
        x = c.read_noerror()
        if x[0]=='won':
            print "won"
            results['winner'] = x[1]
            results['boards'].append(x[2])
            break
        elif x[0]=='update':
            print "update"
            results['boards'].append(x[3])
    #TODO collect all updates and won here
    
    for i in gladiators:
        i.kill()
        i.wait()
        
    return results


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

import subprocess
import random
import os
import os.path
import socket
from time import sleep
from multiprocessing import Process, Queue
import sys

import protocol
from protocol import A
import board



def run_match(server,port,path,bots,max_updates=200,pause=0):
    '''Runs a match between bots
    server:     server's hostname
    port:       server's port
    path:       bot's path
    bots:       list of bot id
    
    This function should collect some stats on the game
    '''
    max_updates*=len(bots)
    game_name = 'automated-match-%u' % random.randint(0,7000000)
    
    results= {}
    results['boards']=[]
    results['players'] = {}
    results['game_name'] = game_name
    
    c = protocol.Client(socket.create_connection((server,port)).makefile())
    
    
    
    logdir='/tmp/%s' % game_name
    os.mkdir(logdir)
    print logdir
    
    gladiators=[]

    #Launches 1st bot and creates the game
    logfile=open('%s/Log-%s'%(logdir,str(bots[0])),'w')
    host_gladiator = subprocess.Popen(['/usr/bin/pypy','--jit', 'threshold=3', path,'-s',server,'-p',str(port),'-H',game_name,'-y',str(len(bots)),'-b',str(bots[0])],
                                      stdout=logfile,stderr=logfile,cwd=os.path.dirname(path))
                                      
    gladiators.append(host_gladiator)

    #Spectate the game
    for count in xrange(60):
        sleep(0.5)
        if count>50:
            print "Game can't be spectated"
            gladiators[0].kill()
            sys.exit(1)
        x=c.list_games()
        if game_name in x[0]:
            break
    c.do_login("watcher-%s" % game_name)
    c.spectate(game_name)
    
    sleep(pause)
    
    #Launch all the other bots
    for i in bots[1:]:
        logfile=open('%s/Log-%s'%(logdir,str(i)),'w')
        proc=subprocess.Popen(['/usr/bin/pypy','--jit', 'threshold=3', path,'-s',server,'-p',str(port),'-g',game_name,'-b',str(i)],
                              stdout=logfile,stderr=logfile,cwd=os.path.dirname(path))
        gladiators.append(proc)
    
    
    for count in xrange(max_updates):
        x = c.read_noerror()
        if x[0]=='won':
            print "won"
            results['winner'] = x[1]
            results['boards'].append(x[2])
            break
        elif x[0]=='update':
            results['players'][x[1][0]] = x[1][1]
            print "update"
            results['boards'].append(x[3])
    
    
    for i in gladiators:
        i.kill()
        i.wait()
        
    return results

def evaluate_match(server,port,path,bots,max_updates=1000,pause=0,queue=None):
    result = run_match(server,port,path,bots,max_updates,pause)
    result['distances']={}
    for i in result['players']:
        result['distances'][i]=(board.euclidean_distance_from_target(result['boards'][-1],i))
    for i in result['players']:
        result['players'][i] = result['players'][i].split('-')[1]
    result['plies'] = len(result['boards'])
    #TODO collect all updates and won here
    if queue==None:
        return result
    queue.put(result)


def parallel_matches(server,port,path,bots,max_updates=1000,pause=0):
    
    matches=[]
    queues=[]
    results=[]
    for b in bots:
        q = Queue()
        p = Process(target = evaluate_match, args=(server,port,path,b,max_updates,pause,q))
        p.start()
        matches.append(p)
        queues.append(q)
    print "ALL LAUNCHED"
    for i in xrange(len(matches)):
        results.append(queues[i].get())
        matches[i].join()
    return results

def gather_stats(addr):
    fd=open('stats.py','a')
    #0               trivial_bot
    #1               static_distance_bot
    #2               iddfs_bot
    #3               static_iddfs_bot
    #4               minimax_bot
    #5               parallel_static_bot
    #6               evolved_distance_bot
    #7               evolved_iddfs_bot
    #8               parallel_euclidean_bot
    #9               parallel_evolved_bot
    matches=(
    
    #(0,0),
    #(1,1),
    #(2,2),  --stuck
    #(2,0),
    (2,1),
    (3,2),
    #(3,3), --stuck
    (3,0),
    (3,1),
    (0,1,2),
    (0,5,9),
    (0,5,3),
    (5,0),
    (5,1),
    #(5,2), --stuck
    (5,3),
    (6,6),
    (6,0),
    (6,1),
    (6,2),
    (6,5),
    (9,5),
    (8,5),
    (9,7),
    (9,9),
    (9,8),
    (5,5),
    
    (0,5,6),
    (0,5,7),
    (8,5,9),
    (3,6,7),
    (9,0,1),
    (0,1,2,3,5,6),
    (0,1,2,3,9,7),
    (0,1,2,3,8,2),
    (0,1,0,1,0,1),
    )
    
    for i in matches:
        for k_ in xrange(2):
            sleep(120)
            r=evaluate_match(addr,8000,'/home/salvo/Documents/uni/artificial/tin171/bot/bot.py',i,pause=0)
            r['boards']=r['boards'][-1]
            fd.write('%s\n' % repr(r))
    fd.close()


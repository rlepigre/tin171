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
import signal

import protocol
from protocol import A
import board

class val:
    def __init__(self):
        self.i=False
interrupt=val()

def handler(signum, frame):
    print 'Signal handler called with signal', signum
    print interrupt.i
    interrupt.i=True
    print interrupt.i

def run_match(server,port,path,bots,max_updates=300,pause=0):
    '''Runs a match between bots
    server:     server's hostname
    port:       server's port
    path:       bot's path
    bots:       list of bot id
    
    This function should collect some stats on the game
    '''
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
        if interrupt.i: #Goes to next match in case of control-c
            interrupt.i=False
            break
        try:
            x = c.read_noerror(timeout=20)
            
            if x[0]=='won':
                print "won"
                results['winner'] = x[1]
                results['boards'].append(x[2])
                break
            elif x[0]=='update':
                results['players'][x[1][0]] = x[1][1]
                print "update",count,max_updates
                results['boards'].append(x[3])
        except:
            print "TIMEOUT!!!"
            break
    
    
    for i in gladiators:
        i.kill()
        i.wait()
        
    return results

def evaluate_match(server,port,path,bots,max_updates=300,pause=0,queue=None):
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


def parallel_matches(server,port,path,bots,max_updates=300,pause=0):
    
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
    fd=open('stats.txt','a')
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
    
    matches=((0, 1),
    (0, 2),
    (0, 3),
    (0, 4),
    (0, 5),
    (0, 6),
    (0, 7),
    (0, 8),
    (0, 9),
    (0, 10),
    (0, 11),
    (0, 12),
    (0, 13),
    (0, 14),
    (0, 15),
    (1, 0),
    (1, 2),
    (1, 3),
    (1, 4),
    (1, 5),
    (1, 6),
    (1, 7),
    (1, 8),
    (1, 9),
    (1, 10),
    (1, 11),
    (1, 12),
    (1, 13),
    (1, 14),
    (1, 15),
    (2, 0),
    (2, 1),
    (2, 3),
    (2, 4),
    (2, 5),
    (2, 6),
    (2, 7),
    (2, 8),
    (2, 9),
    (2, 10),
    (2, 11),
    (2, 12),
    (2, 13),
    (2, 14),
    (2, 15),
    (3, 0),
    (3, 1),
    (3, 2),
    (3, 4),
    (3, 5),
    (3, 6),
    (3, 7),
    (3, 8),
    (3, 9),
    (3, 10),
    (3, 11),
    (3, 12),
    (3, 13),
    (3, 14),
    (3, 15),
    (4, 0),
    (4, 1),
    (4, 2),
    (4, 3),
    (4, 5),
    (4, 6),
    (4, 7),
    (4, 8),
    (4, 9),
    (4, 10),
    (4, 11),
    (4, 12),
    (4, 13),
    (4, 14),
    (4, 15),
    (5, 0),
    (5, 1),
    (5, 2),
    (5, 3),
    (5, 4),
    (5, 6),
    (5, 7),
    (5, 8),
    (5, 9),
    (5, 10),
    (5, 11),
    (5, 12),
    (5, 13),
    (5, 14),
    (5, 15),
    (6, 0),
    (6, 1),
    (6, 2),
    (6, 3),
    (6, 4),
    (6, 5),
    (6, 7),
    (6, 8),
    (6, 9),
    (6, 10),
    (6, 11),
    (6, 12),
    (6, 13),
    (6, 14),
    (6, 15),
    (7, 0),
    (7, 1),
    (7, 2),
    (7, 3),
    (7, 4),
    (7, 5),
    (7, 6),
    (7, 8),
    (7, 9),
    (7, 10),
    (7, 11),
    (7, 12),
    (7, 13),
    (7, 14),
    (7, 15),
    (8, 0),
    (8, 1),
    (8, 2),
    (8, 3),
    (8, 4),
    (8, 5),
    (8, 6),
    (8, 7),
    (8, 9),
    (8, 10),
    (8, 11),
    (8, 12),
    (8, 13),
    (8, 14),
    (8, 15),
    (9, 0),
    (9, 1),
    (9, 2),
    (9, 3),
    (9, 4),
    (9, 5),
    (9, 6),
    (9, 7),
    (9, 8),
    (9, 10),
    (9, 11),
    (9, 12),
    (9, 13),
    (9, 14),
    (9, 15),
    (10, 0),
    (10, 1),
    (10, 2),
    (10, 3),
    (10, 4),
    (10, 5),
    (10, 6),
    (10, 7),
    (10, 8),
    (10, 9),
    (10, 11),
    (10, 12),
    (10, 13),
    (10, 14),
    (10, 15),
    (11, 0),
    (11, 1),
    (11, 2),
    (11, 3),
    (11, 4),
    (11, 5),
    (11, 6),
    (11, 7),
    (11, 8),
    (11, 9),
    (11, 10),
    (11, 12),
    (11, 13),
    (11, 14),
    (11, 15),
    (12, 0),
    (12, 1),
    (12, 2),
    (12, 3),
    (12, 4),
    (12, 5),
    (12, 6),
    (12, 7),
    (12, 8),
    (12, 9),
    (12, 10),
    (12, 11),
    (12, 13),
    (12, 14),
    (12, 15),
    (13, 0),
    (13, 1),
    (13, 2),
    (13, 3),
    (13, 4),
    (13, 5),
    (13, 6),
    (13, 7),
    (13, 8),
    (13, 9),
    (13, 10),
    (13, 11),
    (13, 12),
    (13, 14),
    (13, 15),
    (14, 0),
    (14, 1),
    (14, 2),
    (14, 3),
    (14, 4),
    (14, 5),
    (14, 6),
    (14, 7),
    (14, 8),
    (14, 9),
    (14, 10),
    (14, 11),
    (14, 12),
    (14, 13),
    (14, 15),
    (15, 0),
    (15, 1),
    (15, 2),
    (15, 3),
    (15, 4),
    (15, 5),
    (15, 6),
    (15, 7),
    (15, 8),
    (15, 9),
    (15, 10),
    (15, 11),
    (15, 12),
    (15, 13),
    (15, 14))


    
    for i in matches:
        for k_ in xrange(1):
            print "Relaxing before a new game"
            sleep(3)
            def_handler=signal.signal(signal.SIGUSR1,handler)
            r=evaluate_match(addr,8000,'/home/salvo/Documents/uni/artificial/tin171/bot/bot.py',i,pause=0)
            signal.signal(signal.SIGUSR1,def_handler)
            
            if len(r['boards'])>0: r['boards']=r['boards'][-1]
            fd.write('%s\n' % repr(r))
            fd.flush()
    fd.close()

#evaluate_match('95.80.60.40',8000,'/home/salvo/Documents/uni/artificial/tin171/bot/bot.py',(8,0,1,0),pause=10)
gather_stats('127.0.0.1')


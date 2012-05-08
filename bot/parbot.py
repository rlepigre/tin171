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

from multiprocessing import Process, Queue, Value

from board import *

def reasonable_moves(board,player,distance_function=static_distance_from_target):
    '''returns the most reasonable moves,
    it will return a list of tuples, every
    tuple being (new_board,moves,distance)
    '''
    moves = list(all_moves(board, player))
    boards = [update_board(board,i) for i in moves]
    distances = [distance_function(i,player) for i in boards]
    r=zip(boards,moves,distances)
    r.sort(key=(lambda x: x[2]))
    return r[0:3] #[0:len(r)/5+1] #TODO that 5 is an heuristic

def resulting_boards(board,opponents,distance):
    '''Returns a list of most likely boards from the starting board
    assuming that the bot has just played, opponents is the list of
    other opponents that still need to play
    distance is a distance function
    '''
    res=[board]
    
    for i in opponents:
       _next=[]
       for b in res:
           #print reasonable_moves(b,i)
           _next+=[l[0] for l in reasonable_moves(b,i,distance)]
       res=_next
    
    return res
def points(board,opponents,distance,player_id,r=None):
    '''
    r can be a multiprocessing Value class, to use instead of return
    '''
    res = None
    for i in resulting_boards(board,opponents,distance):
        for move in all_moves(i,player_id):
            d=distance(update_board(i,move), player_id)
            if res==None:
                res=d
            elif res>d:
                res=d
        pass
    
    
    
    if r!=None:
        r.value=res
    return res
def get_opponents(board,player_id):
    '''Returns a tuple of opponents suitable for the other functions'''
    if hasattr(get_opponents,"opp"):
        return get_opponents.opp
    
    l=list(set(board) - set(' #%s' % str(player_id)))
    l.sort()
    l=[int(i) for i in l]
    res = [i for i in l if i > player_id] + [i for i in l if i < player_id]
    get_opponents.opp = res
    return res
    
def parallel_static_bot(c, timeout, board, player_id,distance_function=euclidean_distance_from_target):
    
    key=lambda x:distance_function(update_board(board, x), player_id)
    moves = list(all_moves(board, player_id))
    #random.shuffle(moves) # In this way the bots don't get stuck.
    moves.sort(key=key)
    
    best_move=[moves[0],1,distance_function(update_board(board, moves[0]), player_id)]
    
    opponents=get_opponents(board,player_id)
    
    for depth in xrange(1,2): #TODO more depth?
        workers=[]
        for i in moves[0:5]:
            v=Value('d',0.0)
            p=Process(target = points, args=(update_board(board,i),opponents,distance_function,player_id,v))
            workers.append((p,v,i))
            p.start()
            pass
        for i in workers:
            i[0].join()
            if i[1].value < best_move[2]:
                best_move[0]=i[2]
                best_move[1]=depth+1
                best_move[2]=i[1].value
    
    c.move(best_move[0])
def parallel_euclidean_bot(c,timeout,board,player_id):
    return parallel_static_bot(c,timeout,board,player_id,distance_function=euclidean_distance_from_target)
def parallel_evolved_bot(c,timeout,board,player_id):
    return parallel_static_bot(c,timeout,board,player_id,distance_function=evolved_distance)
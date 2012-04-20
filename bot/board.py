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

def adjacent(n):
    l=[]
    #TODO check borders
    l.append(n+1)
    l.append(n-1)
    l.append(n-18)
    l.append(n-17)
    l.append(n+18)
    l.append(n+17)
    
    return l
    
def next_hole(a,b):
    #TODO
    pass
    
def available_moves(board,initial,first=True,jumped=[]):
    l=[]
    
    for i in adjacent(initial):
        if board[i] == 0 and first:
            l.append(i)
        elif board[i] > 0 and board[next_hole(initial,i)]==0:
            pass
    
    pass
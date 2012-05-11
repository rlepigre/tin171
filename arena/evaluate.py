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

def load_stats(filename):
    fd=open(filename)
    r=fd.readlines()
    fd.close()
    res=[]
    for i in r:
        res.append(eval(i))
    
    return res

def player_plies(stats):
    plies=[None,None,[],[],[],[],[]]
    print "Plies in players"
    for i in stats:
        #print i
        plies[len(i['players'])].append(i['plies'])
        pass
    
    for i in xrange(2,len(plies)):
        if len(plies[i]) == 0: continue
        print i, reduce((lambda x,y:x+y),plies[i],0) / float(len(plies[i]))

def main():        
    stats=load_stats("stats.cpython")
    player_plies(stats)

main()    
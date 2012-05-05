#!/usr/bin/env python
# -*- coding: utf-8 -*-
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

# This program takes a distance array for player 1 (such as generated
# by the genetic algorithm) and rotates it so that it is usable by all
# players.

import BoardWidget, board, math
from numpy import *

def is_invalid(n):
    return board.FULL_BOARD[n] == board.INVALID

def rot(t):
    return mat([[cos(t),-sin(t)],
                [sin(t),cos(t)]])

import sys
print >> sys.stderr, "Type the distance array (for player 1) as a Python array or tuple:"
vec = input()

pos = {}
inv = []
for i, p in BoardWidget._positions.items():
    x, y = p
    o = 500.0
    pos[i] = (x-o, y-o)
    inv.append(((x-o, y-o), i))

def closest(p1, p2):
    """Find the index of the position on the board which is closest to
    the graphical position p1, p2."""
    def dist(pos):
        q1, q2 = pos[0]
        return sqrt((p1 - q1)**2 + (p2 - q2)**2)
    l = list(inv)
    l.sort(key=dist)
    (point, idx) = l[0]
    return idx

def rotate(vec, t):
    """Rotate the distance array vec by the angle t."""
    newvec = [-1] * len(vec)
    M = rot(t)
    for w, i in zip(vec, range(len(vec))):
        if not i in pos or is_invalid(i): continue
        (x,y) = pos[i]
        point = mat([x, y])
        rpoint = point * M
        nidx = closest(rpoint[0,0], rpoint[0,1])
        newvec[nidx] = w
    return newvec

print "data=(None,"
for d in [ rotate(vec, i * 2 * pi/6) for i in [0,3,1,4,5,2] ]:
    print repr(tuple(d)) + ","
print ")"


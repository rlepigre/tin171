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

import BoardWidget, board, math

def is_included(pos):
    # Decides if a position should be drawn with a fill color or not.
    if pos in board.REVERSED_FULL_BOARD[' ']: return True
    if pos in board.REVERSED_FULL_BOARD['1']: return True
    if pos in board.REVERSED_FULL_BOARD['2']: return True
    return False

import sys
print >> sys.stderr, "Type the distance array as a Python array or tuple:"
vec = input()

palette = []
for line in file('/usr/share/gimp/2.0/palettes/Firecode.gpl').readlines()[2:]:
   if line[0]=='#': continue
   palette.append(tuple(map(int,line.split()[0:3])))
for c,i in zip(palette, range(len(palette))):
    r,g,b = c
    print "\\definecolor{C%d}{rgb}{%f,%f,%f}" % (i,r/255.0,g/255.0,b/255.0)

pos = BoardWidget._positions
print
print "%% vec =",repr(vec)
print """\\begin{tikzpicture}"""

lowest = min(vec)
highest = max(vec)

for w, i in zip(vec, range(len(vec))):
    if not i in pos: continue
    (x,y)=pos[i]
    o = 500
    s = 150.0
    color = math.ceil(((w - lowest) / float(highest-lowest)) * (len(palette)-1))
    #color = (math.log(1 + w - lowest) / math.log(1 + highest - lowest)) * (len(palette)-1)
    if is_included(i):
        print "\\filldraw [fill=C%d] (%f,%f) circle (0.08); -- %d" %\
            (color,(x-o)/s,-(y-o)/s,w)
    else:
        print "\\draw [color=gray] (%f,%f) circle (0.08); " % ((x-o)/s,-(y-o)/s)

print """\\end{tikzpicture}"""

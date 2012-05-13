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

# Draw a picture of a Chinese Checkers board

import BoardWidget, board, math

print """\\begin{tikzpicture}"""

colors = {'1':'blue',
          '2':'cyan',
          '3':'red',
          '4':'orange',
          '5':'green',
          '6':'yellow',
          ' ':'white'}

for p, i in zip(board.FULL_BOARD, range(len(board.FULL_BOARD))):
    if p == board.INVALID: continue
    (x,y) = BoardWidget._positions[i]
    o = 500
    s = 150.0
    color = colors[p]
    print "\\filldraw [fill=%s] (%f,%f) circle (0.08);" %\
        (color, (x-o)/s, -(y-o)/s)

print """\\end{tikzpicture}"""

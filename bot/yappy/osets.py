# -*- coding: UTF-8 -*-
"""
This is part of Yappy

osets.py -- a Set private implementation

Copyright (C) 2000-2003 Rogério Reis & Nelma Moreira {rvr,nam}@ncc.up.pt
Version: $Id: osets.py,v 1.3 2004/02/18 10:54:48 rvr Exp $

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   

@author: Rogério Reis & Nelma Moreira {rvr,nam}@ncc.up.pt
"""

class Set(object):
    """ Sets: that is lists without order or repetition.

    May be used everywhere lists are used... because they rely on them."""
    def __init__(self,list=[]):
        foo=[]
        for m in list:
            if not m in foo:
                foo.append(m)
        self.members = foo

    def __getitem__(self, index):
        return self.members[index]

    def __setitem__(self,index,value):
        self.members[index] = value

    def __getattr__(self,name):
        return getattr(self.members, name)
    
    def __add__(self, other):
        new = Set(self.members[:])
        for v in other:
            if not v in new:
                new.append(v)
        return new

    def __iadd__(self, other):
        return self + other

    def __radd__(self,other):
        return self + other
    
    def __sub__(self, other):
        new = Set(self.members[:])
        for v in other:
            try: del(new.members[new.index(v)])
            except ValueError:
                continue
        return new
    
    def __cmp__(self,other):
        if len(self) == len(other):
            if not len(self - other):
                return(0)
        return(1)
        
    def __len__(self):
        return len(self.members)

    def __str__(self):
        return str(self.members)
            
    def __repr__(self):
        return "Set %s"%str(self.members)
    
    def __getslice__(self,low,high):
        return Set(self.members[low:high])

    def __delslice__(self,low,high):
        for i in range(low,max(high+1,len(self.members)-1)):
            del self.members[i]
    
    def __delitem__(self,key):
        del self.members[key]
        
    def append(self,member):
        if not member in self.members:
            self.members.append(member)

    def s_append(self,member):
        e = 0
        if not member in self.members:
            self.members.append(member)        
            e = 1
        return e

    def empty(self):
        return len(self.members) == 0

    def s_extend(self,other):
        e = 0
        for v in other:
            if not v in self:
                self.members.append(v)
                e = 1
        return e 
    def sort(self):
        self.members.sort()

    def index(self, index):
        return self.members.index(index)

    def remove(self,v):
        try: del(self.members[self.index(v)])
        except ValueError:
            pass

    def copy(self):
        return Set(self.members[:])

    def first(self):
        return self.members[0]

    # duplicates a set (shallow copy)
    def dup(self):
        new = Set()
        new.members = self.members[:]
        return new

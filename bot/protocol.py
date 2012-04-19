#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Server protocol parser for the Chinese checkers bot
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

from yappy.parser import *      # apt-get install python-yappy

debug = True

atoms = {}
class Atom():
    def __init__(self, x):
        self.x = x
    def __str__(self):
        return self.x
    def __repr__(self):
        return '"'+ repr(self.x)[1:-1] + '"'
    def __eq__(self,n):
        return str(self) == n

def A(x):
    if not x in atoms: atoms[x] = Atom(x)
    return atoms[x]

# Erlang datum parser. It does not know that strings are lists of
# integers and it does not handle the escapes in strings or atoms, but
# that shouldn't matter for our purposes.
class Parser(Yappy):
    # XXX: for some crazy reason this creates the file "tablereg"
    def __init__(self, no_table=1, table='tablereg'):
        tokenize = [(r'".*?(?<!\\)"', lambda x: ("string", x[1:-1])), #not unescaped
                    (r"'.*?(?<!\\)'", lambda x: ("atom", x[1:-1])), #not unescaped
                    ("\s+", ""),
                    ("[a-z_][A-Za-z0-9_/]*", lambda x: ("atom",x)),
                    ("[0-9]+", lambda x: ("int", x)),
                    ("[{}()\[\],.]",lambda x: (x,x)),
                    ]
        grammar = grules([
                ("exp -> atom", self.Atom),
                ("exp -> int", self.Int),
                ("exp -> string", self.String),
                ("exp -> exp .", self.BaseSemRule),
                # lists
                ("exp -> [ list ", self.List),
                ("list -> exp ]", self.ListEnd),
                ("list -> exp , list", self.ListItem),
                ("list -> ]", self.Eol),
                # tuples
                ("exp -> { tuple ", self.Tuple),
                ("tuple -> exp }", self.ListEnd),
                ("tuple -> exp , tuple", self.ListItem),
                ("tuple -> }", self.Eol),
                ])
        Yappy.__init__(self,tokenize,grammar,table,no_table)
    def BaseSemRule(self,l,context):
        return l[0]
    def Atom(self, l, context):
        return A(l[0])
    def Int(self, l, context):
        return int(l[0])
    def String(self, l, context):
        return l[0]
    def List(self, l, context):
        return l[1]
    def ListItem(self, l, context):
        return [l[0]]+l[2]
    def ListEnd(self, l, context):
        return [l[0]]
    def Eol(self, l, context):
        return []
    def Tuple(self, l, context):
        return tuple(l[1])

# Erlang datum formatter
def Fmt(obj):
    if type(obj) == type(()):
        return '{' + ','.join([Fmt(x) for x in obj]) + '}'
    elif type(obj) == type([]):
        return '[' + ','.join([Fmt(x) for x in obj]) + ']'
    elif type(obj) == type(""):
        return repr([ord(c) for c in obj])
    elif type(obj) == type(0):
        return repr(obj)
    elif obj.__class__ == Atom:
        return "'" + str(obj).replace('\\','\\\\').replace('\'','\\\'') + "'"
    else:
        raise Exception('Unrepresentable object', obj)

# The Chinese Checkers protocol.
class Client():
    def __init__(self, f):
        self.f = f
        self.parser = Parser()

    def read(self):
        """Read a datum from the server."""
        # Yappy's lexer apparently can't consume input by itself, so
        # this evil hack is needed.
        x = self.f.readline()
        if x == '':
            if debug: print '=> (Connection closed by server)'
            return x
        while True:
            try:
                msg = self.parser.input(x)
                if debug: print '=>',msg
                return msg
            except LRParserError, e:
                if not '$' == e.symbol:
                    raise e
            x += self.f.readline()

    def read_noerror(self):
        """Read a datum from the server and raise an exception if the
        the read datum is an error."""
        x = self.read()
        if type(x) == type(()) and x[0] == A('error'):
            raise Exception('Unexpected error from server', x)
        return x

    def send(self, datum):
        """Send a datum to the server."""
        if debug: print '<=', datum
        self.f.write(Fmt(datum) + '.\n')
        self.f.flush()

    def send_ok(self, datum):
        """Send a datum and raise an expection if the reply is not ok."""
        self.send(datum)
        reply = self.read()
        if reply != A('ok'):
            raise Exception('Command failed', datum, reply)
        return reply

    ## High-level protocol.

    def do_login(self, login):
        self.send_ok((A('login'), login))

    def list_games(self):
        self.send(A('list_games'))
        (_, new, running) = self.read_noerror()
        return (new, running)

    def host_game(self, name):
        return self.send_ok((A('host_game'), name))

    def join_game(self, name):
        return self.send_ok((A('join_game'), name))

    def spectate(self, name):
        return self.send_ok((A('spectate'), name))

    def start_game(self):
        self.send_ok(A('start_game'))

    def leave(self):
        self.send_ok(A('leave'))

    def move(self, positions):
        self.send_ok((A('move', positions)))


tests = [('ok',                   A('ok')),
         ("'X'",                  A("X")),
         ('{login, "bot"}.',      (A('login'), "bot")),
         ('{games,[],[]}',        (A('games'), [], [])),
         ('[12,34,08]',           [12,34,8]),
         ('{host_game, "test"}.', (A('host_game'), 'test')),
         ('{player_joined, ["abc","def"]}',
          (A('player_joined'), ['abc', 'def'])),
         ("""{game_start,1,
            [{1,"bot"},{2,"weinholt"}],
            "####1"}""",
          (A('game_start'),
           1,
           [(1,"bot"),(2,"weinholt")],
           "####1"))
         ]

def run_tests():
    p = Parser()
    for test, expect in tests:
        have = p.input(test)
        print have
        if have != expect:
            print have, "!=", expect
    print repr(Fmt((A('login'),
                    1,2,"c")))
    print "tests ok"

if __name__ == "__main__":
    run_tests()

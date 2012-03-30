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

import sys

from PyQt4 import QtCore, QtGui
from PyQt4 import QtNetwork

from BoardWidget import BoardWidget
import gui

class StateEnum:
    DISCONNECTED = 0
    CONNECTED = 1
    WAITING_GAMES = 2
    WAITING_PLAYERS = 3
    WAITING_AUTH = 4
    GAME_STARTED = 5
    

class GameUI(QtGui.QMainWindow):
    
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.ui = gui.Ui_MainWindow()
        self.ui.setupUi(self)
        self.svg = BoardWidget()
        QtCore.QObject.connect(self.svg,QtCore.SIGNAL("clicked(int)"),self.board_click)
        self.ui.boardLayout.addWidget(self.svg)
        
        self.state = StateEnum.DISCONNECTED
        
        #connection to the server
        self.socket=QtNetwork.QTcpSocket()
        QtCore.QObject.connect(self.socket,QtCore.SIGNAL("readyRead()"), self.socket_event)
        QtCore.QObject.connect(self.socket,QtCore.SIGNAL("connected()"), self.socket_connected)
        QtCore.QObject.connect(self.socket,QtCore.SIGNAL("disconnected()"), self.socket_disconnected)
        
    def connect_server(self):
        '''slot connected to the event of button pressed'''
        
        #TODO close the socket if it was trying to connect
        
        hostname = self.ui.txtHostname.text()
        port = self.ui.spinPort.value()
        
        if self.socket.state() != QtNetwork.QAbstractSocket.UnconnectedState:
            self.socket.close()
        
        self.socket.connectToHost(hostname,port,QtNetwork.QAbstractSocket.ReadWrite)
        #self.socket.connectToHost("localhost",9000,)
            
    def socket_event(self):
        '''Slot connected to the signal of the socket'''
        while self.socket.canReadLine():
            msg = self.socket.readLine()
            print msg
            self.new_data(msg)
        
    def new_data(self,msg):
        '''elaborates the messages from the server'''
        if self.state == StateEnum.WAITING_AUTH:
            if msg=="ok":
                self.get_games()
            else:
                #TODO authentication failed...
                pass
        elif self.state == StateEnum.WAITING_GAMES:
            pass
    #CONNECTED = 1
    # = 2
    #DISCONNECTED = 0
    #WAITING_PLAYERS = 3
     #= 4
    #GAME_STARTED = 5
    
    def get_games(self):
        self.state=StateEnum.WAITING_GAMES
        self.socket.write("list_games\n")
    
    def authenticate(self):
        '''sends authentication to the server'''
        self.state=StateEnum.WAITING_AUTH
        
        import os
        import socket

        username = os.getlogin() + "@" + socket.gethostname()
        
        #TODO username could be something else
        
        self.socket.write("login %s\n" % username)
    
    def socket_connected(self):
        
        self.authenticate()
        
        self.ui.txtHostname.setEnabled(False)
        self.ui.spinPort.setEnabled(False)
        self.ui.cmdConnect.setEnabled(False)
        self.ui.cmdDisconnect.setEnabled(True)
        pass
    
    def socket_disconnected(self):
        self.state = StateEnum.DISCONNECTED
        self.socket.close()
        self.ui.txtHostname.setEnabled(True)
        self.ui.spinPort.setEnabled(True)
        self.ui.cmdConnect.setEnabled(True)
        
        self.ui.cmdJoin.setEnabled(False)
        self.ui.cmdSpectate.setEnabled(False)
        self.ui.cmdStart.setEnabled(False)
        self.ui.cmdHost.setEnabled(False)
        
        self.ui.cmdDisconnect.setEnabled(False)
        
    def board_click(self,i):
        #TODO this is just for testing
        self.svg.setMarble(i,1+(i%6))
        
        
        
    def spectate(self):
        pass
    def join(self):
        pass
    def host(self):
        pass
    def start(self):
        pass
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    
    MainWindow = GameUI()
    MainWindow.show()
    sys.exit(app.exec_())


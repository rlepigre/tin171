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
import json

from PyQt4 import QtCore, QtGui
from PyQt4 import QtNetwork

from BoardWidget import BoardWidget
import gui
import protocol
import parser


class StateEnum:
    DISCONNECTED = 0
    CONNECTED = 1
    WAITING_GAMES = 2
    WAITING_PLAYERS = 3
    WAITING_AUTH = 4
    GAME_STARTED = 5
    HOST_OK_WAIT = 6
    JOIN_OK_WAIT = 7
    START_WAIT = 8
    MOVE_WAIT = 9
    SPECTATE_WAIT = 10
    

class GameUI(QtGui.QMainWindow):
    
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.ui = gui.Ui_MainWindow()
        self.ui.setupUi(self)
        self.svg = BoardWidget()
        QtCore.QObject.connect(self.svg,QtCore.SIGNAL("clicked(int)"),self.board_click)
        self.ui.boardLayout.addWidget(self.svg)
        
        self.state = StateEnum.DISCONNECTED
        self.parser = parser.Parser()
        self.board = self.svg.getBoard()
        self.player_id = -1
        self.my_turn = False
        self.steps=[]
        
        
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
            #FIXME ugly hack
            while str(msg).strip().endswith(','):
                msg += self.socket.readLine()
            self.new_data(msg)
        
    def new_data(self,msg):
        '''elaborates the messages from the server'''
        msg=str(msg)
        msg=msg.strip()
        
        print "---->",msg
        msg=self.parser.input(msg)
        print msg
        
        if msg=="ok":
            
            if self.state == StateEnum.MOVE_WAIT:
                last = self.steps[-1]
                self.board[last] = self.player_id
                self.svg.setMarble(last,self.player_id)
                self.steps=[]
            
            elif self.state == StateEnum.WAITING_AUTH:
                self.get_games()
                
                self.ui.cmdJoin.setEnabled(True)
                self.ui.cmdSpectate.setEnabled(True)
                self.ui.cmdStart.setEnabled(True)
                self.ui.cmdHost.setEnabled(True)
            elif self.state in (StateEnum.HOST_OK_WAIT,StateEnum.JOIN_OK_WAIT):
                self.ui.cmdJoin.setEnabled(False)
                self.ui.cmdSpectate.setEnabled(False)
                self.ui.cmdHost.setEnabled(False)
                self.state = StateEnum.WAITING_PLAYERS
            elif self.state == StateEnum.START_WAIT:
                self.ui.cmdStart.setEnabled(False)
                self.state = StateEnum.GAME_STARTED
            elif self.state == StateEnum.SPECTATE_WAIT:
                self.ui.cmdJoin.setEnabled(False)
                self.ui.cmdSpectate.setEnabled(False)
                self.ui.cmdStart.setEnabled(False)
                self.ui.cmdHost.setEnabled(False)
        elif msg[0]=='error':
            if self.state == StateEnum.MOVE_WAIT:
                self.state = -1
                self.my_turn = True
                self.steps=[]
                self.board=list(self.prev_board)
                self.svg.setBoard(self.board)
        elif msg[0]=="games":
            self.ui.lstGames.clear()
            for i in msg[1]:
                self.ui.lstGames.addItem(i)
            pass
        elif msg[0] in ("player_joined","player_left"):
            self.ui.lstPlayers.clear()
            for i in msg[1]:
                self.ui.lstPlayers.addItem(i)
        elif msg[0] == 'game_start':
            self.player_id = msg[1]
            
            palette=QtGui.QPalette()
            palette.setColor(9,self.svg.getColor(msg[1]))
            self.ui.lstPlayers.setPalette(palette)
            
            self.ui.lstPlayers.clear()
            self.pretty_players(msg[2])
            self.board = protocol.get_gui_board(msg[3])
            self.svg.setBoard(self.board)
        elif msg[0] == 'your_turn':
            #TODO timeout msg[1]
            self.my_turn=True
            self.board = protocol.get_gui_board(msg[2])
            self.svg.setBoard(self.board)
            self.prev_board=list(self.board)
        elif msg[0] == 'update':
            self.board = protocol.get_gui_board(msg[3])
            self.svg.setBoard(self.board)
            
            #TODO Highlights the jumps
            #msg[2].pop()
            #for i in msg[2]:
            #    self.svg.setMarble(i,7)
        elif msg[0]== 'game_state':
            self.board = protocol.get_gui_board(msg[2])
            self.svg.setBoard(self.board)
            self.pretty_players(msg[1])
        elif msg[0] == 'won':
            self.board = protocol.get_gui_board(msg[2])
            self.svg.setBoard(self.board)
            
    def get_games(self):
        self.write("list_games.\n")
    
    def authenticate(self):
        '''sends authentication to the server'''
        self.write(protocol.login_message())
        self.state = StateEnum.WAITING_AUTH
    
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
        if self.my_turn == False or self.board[i] not in (self.player_id,0,7):
            return
        
        print i
        
        if len(self.steps)>0 and self.steps[-1]==i:
            #Move made
            message=protocol.move(self.steps)
            self.write(message)
            self.state = StateEnum.MOVE_WAIT
            self.my_turn = False
            return
            pass
        
        self.steps.append(i)
        #TODO this is just for testing
        self.svg.setMarble(i,7)
        
        
        
        
    def spectate(self):
        #TODO
        message=protocol.spectate_game(self.ui.lstGames.currentItem().text())
        self.write(message)
        pass
    def join(self):
        item = self.ui.lstGames.currentItem()
        if item == None: return
        
        message=protocol.join_game(item.text())
        self.write(message)
        self.state = StateEnum.JOIN_OK_WAIT
        pass
    def host(self):
        gname = QtGui.QInputDialog.getText(self,
                    QtGui.QApplication.translate("Form", "Host game"),
                    QtGui.QApplication.translate("Form", "Insert the name for the new game"),
                    QtGui.QLineEdit.Normal,"")
        if not gname[1]:
            return
        message= protocol.host_game_message(str(gname[0]))
        self.write(message)
        self.state = StateEnum.HOST_OK_WAIT
        self.get_games()
        pass
    def start(self):
        self.state = StateEnum.START_WAIT
        message = protocol.start_game()
        self.write(message)
        pass
    
    
    def write(self,message):
        #TODO return to original state if connection fails
        self.socket.write(message)

    def pretty_players(self,l):
        self.ui.lstPlayers.clear()
        for i in l:#msg[2]:
            
            item=QtGui.QListWidgetItem()
                
                
            bcolor=self.svg.getColor(i[0])
            #inverting
            r=255 & ~bcolor.red()
            g=255 & ~bcolor.green()
            b=255 & ~bcolor.blue()
            fcolor=QtGui.QColor(r,g,b)
                
                
            bbrush=QtGui.QBrush()
            fbrush=QtGui.QBrush()
                
            bbrush.setColor(bcolor)
            fbrush.setColor(fcolor)
                
            bbrush.setStyle(1)
            fbrush.setStyle(1)
                
            item.setBackground(bbrush)
            item.setForeground(fbrush)
                
            item.setText(i[1])
                
            self.ui.lstPlayers.addItem(item)

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    
    MainWindow = GameUI()
    MainWindow.show()
    sys.exit(app.exec_())


import sys

from PyQt4 import QtCore, QtGui
from PyQt4 import QtNetwork

from BoardWidget import BoardWidget
import gui

class GameUI(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.ui = gui.Ui_MainWindow()
        self.ui.setupUi(self)
        self.svg = BoardWidget()
        QtCore.QObject.connect(self.svg,QtCore.SIGNAL("clicked(int)"),self.board_click)
        self.ui.boardLayout.addWidget(self.svg)
        
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
        print "-"
        '''Slot connected to the signal of the socket'''
        #print "-" , self.socket.readLine()
        #TODO should do something here
    def socket_connected(self):
        self.ui.txtHostname.setEnabled(False)
        self.ui.spinPort.setEnabled(False)
        self.ui.cmdConnect.setEnabled(False)
        pass
    def socket_disconnected(self):
        pass
    def board_click(self,i):
        #TODO this is just for testing
        self.svg.setMarble(i,1+(i%6))
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    
    MainWindow = GameUI()
    MainWindow.show()
    sys.exit(app.exec_())


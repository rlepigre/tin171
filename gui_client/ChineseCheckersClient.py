import sys

from PyQt4 import QtCore, QtGui
from PyQt4 import QtSvg

from BoardWidget import BoardWidget
import gui

class GameUI(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        self.ui = gui.Ui_MainWindow()
        self.ui.setupUi(self)
        self.svg = BoardWidget()
        self.ui.boardLayout.addWidget(self.svg)
        
        a=[]
        for i in xrange(300): a.append(0)
        #a[79]=4
        self.svg.setBoard(a)
        

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    
    MainWindow = GameUI()
    MainWindow.show()
    sys.exit(app.exec_())


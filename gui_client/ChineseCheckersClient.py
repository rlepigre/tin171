import sys

from PyQt4 import QtCore, QtGui
from PyQt4 import QtSvg

from BoardWidget import BoardWidget
import gui


if __name__ == "__main__":
    
    
    app = QtGui.QApplication(sys.argv)
    
    MainWindow = QtGui.QMainWindow()
    ui = gui.Ui_MainWindow()
    ui.setupUi(MainWindow)
    
    
    svg = BoardWidget()
    print ui,dir(svg)
    svgPolicy = QtGui.QSizePolicy()
    svgPolicy.setHeightForWidth(True)
    svg.setSizePolicy(svgPolicy)
    
    
    ui.boardLayout.addWidget(svg)
    
    
    a=[]
    for i in xrange(300): a.append(0)

    svg.set_board(a)
    
    
    MainWindow.show()
    sys.exit(app.exec_())


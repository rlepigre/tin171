from PyQt4 import QtSvg,QtCore,QtGui

from math import sqrt

_positions = {4: (500, 243.65648), 21: (481.5, 275.69942), 22: (518.5, 276.46937), 38: (463, 307.74236), 39: (500, 307.74236), 40: (537, 307.74236), 55: (444.5, 339.7853), 56: (481.5, 339.7853), 57: (518.5, 339.7853), 58: (555.5, 339.7853), 68: (278, 371.82823), 69: (315, 371.82823), 70: (352, 371.82823), 71: (389, 371.82823), 72: (426, 371.82824), 73: (463, 371.82824), 74: (500, 371.82824), 75: (537, 371.82824), 76: (574, 371.82824), 77: (611, 371.82824), 78: (648, 371.82824), 79: (685, 371.82824), 80: (722, 371.82824), 86: (296.5, 403.87117), 87: (333.5, 403.87117), 88: (370.5, 403.87117), 89: (407.5, 403.87118), 90: (444.5, 403.87118), 91: (481.5, 403.87118), 92: (518.5, 403.87118), 93: (555.5, 403.87118), 94: (592.5, 403.87118), 95: (629.5, 403.87118), 96: (666.5, 403.87118), 97: (703.5, 403.87118), 104: (315, 435.91411), 105: (352, 435.91411), 106: (389, 435.91412), 107: (426, 435.91412), 108: (463, 435.91412), 109: (500, 435.91412), 110: (537, 435.91412), 111: (574, 435.91412), 112: (611, 435.91412), 113: (648, 435.91412), 114: (685, 435.91412), 122: (333.5, 467.95705), 123: (370.5, 467.95706), 124: (407.5, 467.95706), 125: (444.5, 467.95706), 126: (481.5, 467.95706), 127: (518.5, 467.95706), 128: (555.5, 467.95706), 129: (592.5, 467.95706), 130: (629.5, 467.95706), 131: (666.5, 467.95706), 140: (352, 500), 141: (389, 500), 142: (426, 500), 143: (463, 500), 144: (500, 500), 145: (537, 500), 146: (574, 500), 147: (611, 500), 148: (648, 500), 157: (333.49998, 532.04295), 158: (370.5, 532.04294), 159: (407.5, 532.04294), 160: (444.5, 532.04294), 161: (481.5, 532.04294), 162: (518.5, 532.04294), 163: (555.5, 532.04294), 164: (592.5, 532.04294), 165: (629.5, 532.04294), 166: (666.5, 532.04295), 174: (314.99998, 564.08589), 175: (351.99998, 564.08589), 176: (389, 564.08588), 177: (426, 564.08588), 178: (463, 564.08588), 179: (500, 564.08588), 180: (537, 564.08588), 181: (574, 564.08588), 182: (611, 564.08588), 183: (648, 564.08589), 184: (685, 564.08589), 191: (296.49998, 596.12883), 192: (333.49998, 596.12883), 193: (370.49998, 596.12883), 194: (407.5, 596.12882), 195: (444.5, 596.12882), 196: (481.5, 596.12882), 197: (518.5, 596.12882), 198: (555.5, 596.12882), 199: (592.5, 596.12882), 200: (629.5, 596.12883), 201: (666.5, 596.12883), 202: (703.5, 596.12883), 208: (277.99998, 628.17177), 209: (314.99998, 628.17177), 210: (351.99998, 628.17177), 211: (388.99998, 628.17177), 212: (426, 628.17176), 213: (463, 628.17176), 214: (500, 628.17176), 215: (537, 628.17176), 216: (574, 628.17176), 217: (611, 628.17177), 218: (648, 628.17177), 219: (685, 628.17177), 220: (722, 628.17177), 230: (444.5, 660.2148), 231: (481.5, 660.2148), 232: (518.5, 660.2148), 233: (555.5, 660.2148), 248: (463, 692.25774), 249: (500, 692.25774), 250: (537, 692.25774), 266: (481.5, 724.30068), 267: (518.5, 724.30068), 284: (500, 756.34362)}
_colors=("WhiteHole","RedBall","OrangeBall","YellowBall","GreenBall","BlueBall","PurpleBall")

_board = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n<svg\n\txmlns:svg="http://www.w3.org/2000/svg"\n\txmlns="http://www.w3.org/2000/svg"\n\txmlns:xlink="http://www.w3.org/1999/xlink"\n\tversion="1.0"\n\tviewBox="200 200 600 600">\n\t<title>Chinese Checkers Board</title>\n\t<defs>\n\t\t<linearGradient id="LinearGradientWhite">\n\t\t\t<stop style="stop-color:#ffffff;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#e8e8e8;stop-opacity:1" offset="0.8" />\n\t\t\t<stop style="stop-color:#c0c0c0;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientRed">\n\t\t\t<stop style="stop-color:#ff9999;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#d32d2d;stop-opacity:1" offset="0.8" />\n\t\t\t<stop style="stop-color:#b40000;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientOrange">\n\t\t\t<stop style="stop-color:#ffe4c1;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#ff9a19;stop-opacity:1" offset="0.72" />\n\t\t\t<stop style="stop-color:#f08800;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientYellow">\n\t\t\t<stop style="stop-color:#fffbc1;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#e7dc29;stop-opacity:1" offset="0.75" />\n\t\t\t<stop style="stop-color:#c8bc00;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientBlue">\n\t\t\t<stop style="stop-color:#b9b9ff;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#3131cf;stop-opacity:1" offset="0.8" />\n\t\t\t<stop style="stop-color:#0000b4;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientGreen">\n\t\t\t<stop style="stop-color:#00eb00;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#00b600;stop-opacity:1" offset="0.5" />\n\t\t\t<stop style="stop-color:#005000;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<linearGradient id="LinearGradientPurple">\n\t\t\t<stop style="stop-color:#ebcef2;stop-opacity:1" offset="0" />\n\t\t\t<stop style="stop-color:#ac37c9;stop-opacity:1" offset="0.8" />\n\t\t\t<stop style="stop-color:#79278d;stop-opacity:1" offset="1" />\n\t\t</linearGradient>\n\t\t<filter id="StoneDropShadow" x="-0.25" y="-0.25" width="1.5" height="1.5" color-interpolation-filters="sRGB">\n\t\t\t<feGaussianBlur result="blur" stdDeviation="2" in="SourceAlpha" />\n\t\t\t<feColorMatrix values="1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0.4 0 " type="matrix" result="bluralpha" />\n\t\t\t<feOffset result="offsetBlur" dy="2" dx="2" in="bluralpha" />\n\t\t\t<feMerge><feMergeNode in="offsetBlur" /><feMergeNode in="SourceGraphic" /></feMerge>\n\t\t</filter>\n\t\t<radialGradient id="RadialGradientWhite" xlink:href="#LinearGradientWhite"\n\t\t\tcx="0" cy="0" r="16" fx="8" fy="8" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientRed" xlink:href="#LinearGradientRed"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientOrange" xlink:href="#LinearGradientOrange"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientYellow" xlink:href="#LinearGradientYellow"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientGreen" xlink:href="#LinearGradientGreen"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientBlue" xlink:href="#LinearGradientBlue"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<radialGradient id="RadialGradientPurple" xlink:href="#LinearGradientPurple"\n\t\t\tcx="0" cy="0" r="16" fx="-6" fy="-6" gradientUnits="userSpaceOnUse" />\n\t\t<circle id="WhiteHole" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientWhite);stroke:#cccccc;stroke-width:2" />\n\t\t<circle id="RedBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientRed);stroke:#999999;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t\t<circle id="OrangeBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientOrange);stroke:#aaaaaa;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t\t<circle id="YellowBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientYellow);stroke:#bbbbbb;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t\t<circle id="GreenBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientGreen);stroke:#999999;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t\t<circle id="BlueBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientBlue);stroke:#999999;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t\t<circle id="PurpleBall" cx="0" cy="0" r="16"\n\t\t\tstyle="fill:url(#RadialGradientPurple);stroke:#999999;stroke-width:1;filter:url(#StoneDropShadow)" />\n\t</defs>\n\t<path id="Mainstar" style="fill:#e6e6e6;stroke:#808080;stroke-width:5;filter:url(#StoneDropShadow)"\n\t\td="m 258.55302,347.00013 c -11.77794,0 -17.88058,10.32639 -11.77795,20.4 L 313.3409,482.6955 c 6.93083,12.00455 6.93082,22.60469 0,34.60924 l -66.56583,115.29539 c -5.88897,10.19999 0,20.4 11.77795,20.4 l 133.13166,0 c 13.86164,0 23.04164,5.30007 29.97247,17.30462 l 66.5658,115.29538 c 5.94943,10.16486 17.66693,10.2 23.5559,0 l 66.56583,-115.29539 c 6.93082,-12.00455 16.11081,-17.30462 29.97247,-17.30462 l 133.13164,0 c 11.77794,0 17.66692,-10.20001 11.77795,-20.4 L 686.6609,517.30474 c -6.93082,-12.00455 -6.93083,-22.60469 0,-34.60924 l 66.56584,-115.29537 c 5.88897,-10.19999 0,-20.4 -11.77795,-20.4 l -133.13166,0 c -13.86165,0 -23.04165,-5.30007 -29.97248,-17.30462 l -66.5658,-115.29538 c -5.88897,-10.2 -17.66693,-10.2 -23.5559,0 l -66.56582,115.29539 c -6.93083,12.00455 -16.11082,17.30462 -29.97247,17.30462 z" />\n\t<g id="GroupWhite">\n\t\t%%0%%\n\t</g>\n\t<g id="GroupRed">\n\t\t%%1%%\n\t</g>\n\t<g id="GroupOrange">\n        %%2%%\n\t</g>\n\t<g id="GroupYellow">\n        %%3%%\n\t</g>\n\t<g id="GroupGreen">\n        %%4%%\n\t</g>\n\t<g id="GroupBlue">\n        %%5%%\n\t</g>\n\t<g id="GroupPurple">\n        %%6%%\n\t</g>\n</svg>\n'

def gen_board(board):
    '''Generates the svg XML to represent the given board.
    board has to be an iterable of at least 288 positions'''
    
    groups={}
    for i in xrange(7):
        groups[i]=""
    
    for i in xrange(288):
        if i in _positions:
            ball= '<use xlink:href="#%s" transform="translate%s" />' % (_colors[board[i]],str(_positions[i]))
            groups[board[i]]+=ball
    
    result=_board
    for i in groups:
        result=result.replace("%%%%%d%%%%" % i, groups[i])
    
    return result


class BoardWidget(QtSvg.QSvgWidget):
    
    clicked = QtCore.pyqtSignal(int, name='clicked')
    '''This widgets shows a chinese checkers board.
    The board is shown after the setBoard method has been called'''
    def __init__(self):
        super(BoardWidget, self).__init__()
        svgPolicy = QtGui.QSizePolicy()
        svgPolicy.setHeightForWidth(True)
        self.setSizePolicy(svgPolicy)
        
        self.board=[]
        for i in xrange(300): self.board.append(0)
        self.setBoard(self.board)
        
    def setBoard(self,board):
        self.board=board
        xml=QtCore.QByteArray(gen_board(board))
        return self.load(xml)
    def heightForWidth(self,w):
        return w
        
    def mousePressEvent(self,ev):
        
        dist= lambda c1,c2: sqrt(((c1[0]-c2[0])**2) + ((c1[1]-c2[1])**2))
        
        #TODO those coordinates don't rescale
        coord=(ev.x()+200 , ev.y()+200)
        for i in _positions:
            if dist(coord,_positions[i]) < 20:
                #print _positions[i] ,
                print i
                self.clicked.emit(i)
                
                #TODO this is just for testing
                self.board[i]= 1+(i%4)
                self.setBoard(self.board)
                return
        pass
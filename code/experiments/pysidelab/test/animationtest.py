import sys
from PySide.QtCore import Slot
from PySide import QtCore, QtGui


class RenderArea(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.pen = QtGui.QPen(QtCore.Qt.red)
        self.brush = QtGui.QBrush()

        self.setBackgroundRole(QtGui.QPalette.Base)

        self.vx = 3
        self.vy = 1
        self.px = 10
        self.py = 10
        self.timer = QtCore.QTimer(self)
        self.connect(self.timer, QtCore.SIGNAL("timeout()"), self.tick)
        self.timer.start(20)

    @Slot()
    def tick(self):
        self.px += self.vx
        self.py += self.vy

        if self.px > self.width() - 10 or self.px < 10:
            self.vx *= -1
        if self.py > self.height() -10 or self.py < 10:
            self.vy *= -1
        self.update()

    def minimumSizeHint(self):
        return QtCore.QSize(100, 100)

    def sizeHint(self):
        return QtCore.QSize(400, 200)

    def paintEvent(self, event):
        """
        :type event: QtGui.QPaintEvent
        """
        painter = QtGui.QPainter(self)
        painter.setPen(self.pen)
        painter.setBrush(self.brush)

        self.px = max(10, min(self.width() -10, self.px))
        self.py = max(10, min(self.height()- 10, self.py))

        painter.drawEllipse(QtCore.QPoint(self.px,self.py), 10, 10)





class AnimationWindow(QtGui.QWidget):

    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.renderArea = RenderArea()

        mainLayout = QtGui.QVBoxLayout()
        mainLayout.addWidget(self.renderArea)
        self.setLayout(mainLayout)

        self.setWindowTitle(self.tr("Animation"))

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    window = AnimationWindow()
    window.show()
    sys.exit(app.exec_())


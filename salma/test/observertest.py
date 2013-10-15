'''
Created on 23.09.2013

@author: christian
'''
class Observable(object):
    def __init__(self):
        self.__observers = []
        self.x = 12
    
    def addObserver(self, observer):
        self.__observers.append(observer)
    
    def notifyObservers(self, data):
        for o in self.__observers:
            o(data)
    def setX(self, newX):
        self.x = newX
        self.notifyObservers(newX)
    
class MyObserver(object):
    def __init__(self, name):
        self.__name = name
        
    def update(self, data):
        print("Observer {}: {}".format(self.__name, data))
    
    def register(self, observable):
        '''
        :param observable: Observable 
        '''
        observable.addObserver(self.update)
    
if __name__ == "__main__":
    o = MyObserver("obs1")
    observable = Observable()
    #o.register(observable)
    observable.addObserver(MyObserver.update)
    
    observable.setX(42)

    
    
        
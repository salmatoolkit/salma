'''
Created on 28.05.2013

@author: kroiss
'''

class Foo(object):
    def __init__(self, id):
        self.__id = id
    
    def printFoo(self):
        print("Foo no. %d" % self.__id)
        

if __name__ == "__main__":
    foo = Foo(1)
    foo.printFoo()
    d = dict()
    d[foo] = 42
    print( d[foo])
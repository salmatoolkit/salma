'''
Created on 18.07.2013

@author: kroiss
'''

class ClassTest(object):
    def __init__(self, num):
        self.__num = num
        
    @property
    def num(self):
        return self.__num
    
    @num.setter
    def num(self, value):
        self.__num = value
        print("Set num to %d\n" % value)
      
def foo(name, *params):
    print(len(params))
    print(params)
  
if __name__ == "__main__":
    c = ClassTest(42)
    print(c.num)
    c.num = 50
    
    print(c.num)
    foo("a")
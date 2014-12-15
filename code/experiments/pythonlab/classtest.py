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

class Foo:
    def foo(self, msg):
        pass

class A(Foo):
    def foo(self, msg):
        print("A-foo: " + msg)




class B(Foo):

    def __init__(self, name):
        super().__init__()
        self.name = name
        self.foo = self.__nodef()

    def __nodef(self):
        def __f(m):
            print("OOPS! " + self.name + " - " + m)
        return __f

    def prepare(self):
        self.foo = self.__foo()

    def __foo(self):
        def __f(msg):
            print("B-foo: " + self.name + " - " + msg)
        return  __f

class C:
    __statprop = 42

    @staticmethod
    def set_statprop(p):
        C.__statprop = p

    @property
    def prop(self):
        return self.__statprop


    @staticmethod
    def statprop():
        return C.__statprop

    def complex_prop(self):
        """
        :rtype (int, B)
        """
        return 42, B("chris")
def foo(name, *params):
    print(len(params))
    print(params)
  
if __name__ == "__main__":
    c = ClassTest(42)
    print(c.num)
    c.num = 50
    
    print(c.num)
    foo("a")

    b = B("chris")
    b.foo("hi")
    b.prepare()
    b.foo("hi")

    c1 = C()
    c2 = C()
    print(c1.prop)
    print(c2.prop)
    C.set_statprop(100)
    print(c1.prop)
    print(c2.prop)
    print(C.statprop)

    print(c1.complex_prop()[1].name)
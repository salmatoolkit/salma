SPECIES = "homo sapiens" 
class Person(object):
    
    
    def __init__(self, name, age):
        self.__name = name
        self.__age = age
  
        
    @property
    def name(self):
        return self.__name
    
    @name.setter
    def name(self, newName):
        self.__name = newName

    def __str__(self, *args, **kwargs):
        return "{} {}".format(SPECIES, self.__name) 

class Man(Person):
    def __init__(self, hasBeard, name, age):
        Person.__init__(self, name, age)
        self.__hasBeard = hasBeard
    @property
    def hasBeard(self):
        return self.__hasBeard
  
  

var2 = 42 
def foo(var1):
    #print("Foo says: {} - {}".format(var1, var2))
    global var2
    var2 = 100
    
    
def bar(x,y, z = 10, v = 4, **kwargs):
    print(x,y,z,v,kwargs)
         
if __name__ == "__main__":
    chris = Person("Chris", 33)
    print(chris.name)
    chris.name = "Chrissi"
    print(chris.name)
    print(chris)
    theMan = Man(True, "Chris", 33)
    print(theMan.name, theMan.hasBeard)
    bar(y=2,x=1,z=7, b=8)
    foo(12)
    print(str(var2))
    
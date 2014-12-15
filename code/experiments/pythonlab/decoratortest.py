'''
Created on 28.05.2013

@author: kroiss
'''


def megafunction(x):
    def __newfun(f):
        def __new2fun(*params):
            print("%s is a mega-function with param %d!" % (f.__name__, x))
            f(*params)
        return __new2fun
    return __newfun

def superfunction(f):
    def __newfun(*params):
        print("%s is a super-function!" % f.__name__)
        f(*params)
    return __newfun

@superfunction
def foo(x):
    print(x)
 
 
def bar(*params, **kwargs):
    msg = kwargs['msg'] if 'msg' in kwargs else 'end'
    p = list(params)
    p.append(msg)
    print(p)    


@megafunction(10)
def foo2(x):
    print("Foo2: %d" % x)


foo("hello %s" % "world")
bar(1,2,3)

bar(1,2,3, msg='bla')

bar([1,2,3])
bar(*[1,2,3])

foo2(3)
'''
Created on 28.05.2013

@author: kroiss
'''


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
    
foo("hello %s" % "world")
bar(1,2,3)

bar(1,2,3, msg='bla')

bar([1,2,3])
bar(*[1,2,3])
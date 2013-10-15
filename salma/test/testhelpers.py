'''
Created on 30.07.2013

@author: kroiss
'''

def withHeader(fun):
    def __f(*args):
        print("\n\n")
        print("-" * 80)
        print(fun.__name__)
        print("-" * 80)
        print("\n\n")
        fun(*args)
        
    return __f
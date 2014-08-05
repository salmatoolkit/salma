"""
@author: kroiss
"""

def withHeader(msg=None):
    def __outer(fun):
        def __inner(*args):
            print("\n\n")
            print("-" * 80)
            print(fun.__name__)
            print("-" * 80)
            if msg is not None:
                print(msg)
                print("-" * 80)
            print("\n\n")
            fun(*args)
        return __inner
    return __outer
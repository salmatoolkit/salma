__author__ = 'christian'

import unittest


class Agent(object):
    def __init__(self, name):
        self.name = name
    def __setattr__(self, key, value):
        print("setting {} = {}".format(key,value))
        object.__setattr__(self, key, value)

class AddMethodTest(unittest.TestCase):
    def test_something(self):
        a1 = Agent("chris")
        #print(str(a1))
        #def foo(obj):
        #    print(obj.name)
        #Agent.dostuff = foo
        #a1.dostuff()
        #a1.x = 10
        #print(a1.x)
        print(a1.name)



if __name__ == '__main__':
    unittest.main()

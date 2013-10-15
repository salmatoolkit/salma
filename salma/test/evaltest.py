'''
Created on 18.09.2013

@author: christian
'''

fluents = dict()

fluents[('xpos', ('rob1',))] = 10
fluents[('xpos', ('rob2',))] = 20
fluents[('ypos', ('rob1',))] = 30
fluents[('ypos', ('rob2',))] = 40





def makeFluentFunction(fluentName):
    def __f(*params):
        return fluents[(fluentName, tuple(params))]
    
    return __f 

ctx = {
       'xpos' : makeFluentFunction('xpos'),
       'ypos' : makeFluentFunction('ypos')
       }


# xpos = makeFluentFunction('xpos')
# 
# print("xpos: {}".format(xpos('rob1')))
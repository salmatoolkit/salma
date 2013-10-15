'''
Created on 29.05.2013

@author: kroiss
'''
import pyclp

print("-----------")

pyclp.init()

v = pyclp.Var()
c = pyclp.Compound("compile_term",
                   pyclp.Compound(":-",
                                  pyclp.Compound("foo", v),
                                  pyclp.Compound("=", v, pyclp.Atom("bar"))
                                  ))
print(str(c))

c.post_goal()
result, dummy = pyclp.resume()
if result != pyclp.SUCCEED:
    raise(Exception("Error compiling"))


v2 = pyclp.Var()
c2 = pyclp.Compound("foo",v2)
c2.post_goal()
result, dummy = pyclp.resume()
if result != pyclp.SUCCEED:
    raise(Exception("Error evaluating foo(X)"))

print("X = %s" % str(v2.value())) 


v = pyclp.Var()
c = pyclp.Compound(">", v, 10)
c.post_goal()
result, dummy = pyclp.resume()
if result == pyclp.FLUSHIO:
    outStream=pyclp.Stream(dummy)                # Open output stream
    data=outStream.readall()                    # Return data in a bytes object
    print(data)
    outStream.close()        
elif result == pyclp.SUCCEED:
    print(v.value())
    


pyclp.cleanup()
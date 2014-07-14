__author__ = 'christian'

def genTest(n):
    if n < 0:
        yield (n,n)
        return
    for i in range(n):
        for j in range(n):
            yield (i,j)



def makeGreeter(name):
    __msg = "Hello " + name
    def __greeter(my_name):
        print(__msg + " here is " + my_name)
    return __greeter


def iter2(val):
   i = val
   if val > 3 and i < 20:
       yield i
       i += 1
   else:
       return



for x in genTest(5):
    print("{} - {}".format(x[0],x[1]))
l = genTest(6)
print(l.__next__())
print(l.__next__())
# g = makeGreeter("Mary")
# g("Pete")

l = list(iter2(1))
print(l)
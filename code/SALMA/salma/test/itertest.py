__author__ = 'christian'

def genTest(n):
    if n < 0:
        yield (n,n)
        return
    for i in range(n):
        for j in range(n):
            yield (i,j)





for x in genTest(-5):
    print("{} - {}".format(x[0],x[1]))
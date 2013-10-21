__author__ = 'christian'

def genTest():
    for i in range(10):
        for j in range(10):
            yield (i,j)


for x in genTest():
    print("{} - {}".format(x[0],x[1]))
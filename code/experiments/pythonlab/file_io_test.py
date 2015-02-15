import time
import io

def test1():
    c1 = time.clock()
    with open("test1.txt",mode="w",encoding="UTF-8") as f:
        for i in range(100000):
            row = []
            for j in range(25):
                row.append(i*j)
            
            s = ";".join(map(lambda x : str(x), row)) + "\n"
            f.write(s)
            
    c2 = time.clock()
    print("Time 1: {}".format(c2-c1))
    
def test2():
    c1 = time.clock()
    f = io.FileIO("test2.txt",mode="w")
    buf = io.BufferedWriter(f, buffer_size = 8*io.DEFAULT_BUFFER_SIZE)
    for i in range(100000):
        row = []
        for j in range(25):
            row.append(i*j)
        
        s = ";".join(map(lambda x : str(x), row)) + "\n"
        buf.write(s.encode(encoding="ascii"))
        
    f.close()
    c2 = time.clock()
    print("Time 2: {}".format(c2-c1))    
    
if __name__ == '__main__':
    test1()
    test2()
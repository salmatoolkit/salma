from pyclp import *
print ("I/O Example")
init()                                # Init ECLiPSe engine
stdin=Stream('stdin')                 # Open stdin stream
stdout=Stream('stdout')               # Open stdout stream
A_var=Var()                           # Variable where to store the input term.
Compound ("read",A_var).post_goal()   # read(A)
Compound("writeln",A_var).post_goal() # writeln(A)

while (1):
    result,stream=resume()
    if result==WAITIO:                # ECLiPSe is waiting for data
        stdin.write(b'Hello_World')
    elif result==FLUSHIO:
        print(stdout.readall())       # ECLiPSe send data to stdout stream
    else:
        break
stdin.close()                         # Not required but implemented to support RawIO protocol
stdout.close()                        # Not required but implemented to support RawIO protocol
cleanup()     
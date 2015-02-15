import matplotlib.pyplot as plt
from matplotlib.path import Path
import matplotlib.patches as patches
import matplotlib.collections as coll
import time
import matplotlib.animation as animation

fig = plt.figure()
ax = fig.add_subplot(111)
ax.set_xlim(0, 200)
ax.set_ylim(0, 200)
#: :type ax: Axes

c = patches.Circle((50,50),15)
a = patches.Arrow(100,100,20,20)
#p = coll.PatchCollection([c])
c.center = (100,100)
ax.add_patch(c)
ax.add_patch(a)
def animate(i):
    c.center = (i,100)
    #c.xy = (i,100)
    pass
 
ani = animation.FuncAnimation(fig, animate, 100, repeat=False,
    interval=50)

plt.show()


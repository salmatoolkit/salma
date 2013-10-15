'''
Created on 23.09.2013

@author: christian
'''

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from salma.test.positionplotter import NUM_OF_ROBOTS

NUM_OF_ROBOTS=5

def load():
    types = [("step", "int")]
    for i in range(NUM_OF_ROBOTS):
        types.append(("x" + str(i+1), "float"))
        types.append(("y" + str(i+1), "float"))
        types.append(("dir" + str(i+1), "float"))
    data = np.genfromtxt('robotic_scenario_positions_5robots.csv', dtype=types,
                              delimiter=";")
  
    return data


def update_line(num, d, lines):
    for i in range(0,len(lines)):
        start = 2*i
        end = start+2
        lines[i].set_data(d[start:end,:num])
        
    return tuple(lines)


fig1 = plt.figure()
plt.xlim(0, 500)
plt.ylim(0, 500)
data = load()


lines = []
rows = []
for i in range(0,NUM_OF_ROBOTS):
    rows.append(data['x' + str(i+1)])
    rows.append(data['y' + str(i+1)])
    l, = plt.plot([], [])
    lines.append(l)

drob = np.array(rows)

line_ani = animation.FuncAnimation(fig1, update_line, None, fargs=(drob, lines),
    interval=50, blit=True)
plt.show()
'''
Created on 23.09.2013

@author: christian
'''

import numpy as np
import matplotlib.pyplot as plt
NUM_OF_ROBOTS = 5

def load():
    types = [("step", "int")]
    for i in range(NUM_OF_ROBOTS):
        types.append(("x" + str(i+1), "float"))
        types.append(("y" + str(i+1), "float"))
        types.append(("dir" + str(i+1), "float"))
    
    data = np.genfromtxt('robotic_scenario_positions_5robots.csv', dtype=types,
                              delimiter=";")
    print(data)
    return data

if __name__ == '__main__':
    data = load()
    fig1 = plt.figure()
    plt.xlim(0, 500)
    plt.ylim(0, 500)
    plt.rc('axes', color_cycle=['r', 'g', 'b', 'y'])
    for i in range(NUM_OF_ROBOTS):
        plt.scatter(data['x' + str(i+1)], data['y' + str(i+1)])
    
    plt.show()
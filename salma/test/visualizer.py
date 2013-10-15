'''
Created on 05.09.2013

@author: christian
'''

import numpy as np
import matplotlib.pyplot as plt
import os

def visualizeExperiment(fileName):
    # parse header
    pDefect = None
    P_DEFECT, TOLERANCE, ALPHA, BETA = (None, None, None, None)
    with open(fileName) as f:
        line = f.readline()
        P_DEFECT, TOLERANCE, ALPHA, BETA = list(map(float, line[2:].split(';')))
    data = np.genfromtxt(fileName, dtype=[
                                          ("step", "int"), 
                                          ("p0", "float"), 
                                          ("p1", "float"), 
                                          ("accepted", "bool"), 
                                          ("m", "int"),
                                          ("duration","float")
                                          ],
                          delimiter=";", skip_header=1)
    
    print(data)
    print("P_DEFECT: ",P_DEFECT)
    p = data['p0'] + (data['p1'] - data['p0']) / 2.0
    
    fig =  plt.figure(figsize=(10,5))
    
    #plt.subplot(2,1,1)
    plt.grid(True)
    plt.plot(p, data['m'], label="Z", color="black")
    
    
    
    
    success = np.zeros(data.size)
    success[data['accepted'].nonzero()] = 1.0
#     ax = plt.subplot(2,1,2)
#     ax.set_ylim(-1,2)
#     ax.plot(p, success, drawstyle='steps-pre', label="success")
    
    plt.axvline([P_DEFECT], color='black', linestyle=':', linewidth=1.5)
    plt.annotate("$p_{v}$",(P_DEFECT+0.01,20), fontsize=12)
    plt.grid(False)
    plt.xlabel("$p_{max}$", fontsize=15)
    plt.ylabel("$m$", fontsize=15)
    plt.figaspect(0.5)
    outpath = os.path.abspath("../../../papers/kroiss_aamas2014/seq_test_experiment_uncropped.pdf")
    print(outpath)
    print(os.path.exists(outpath))
    plt.savefig(outpath)
    plt.show()
   
    

if __name__ == "__main__":
    #visualizeExperiment("seqtest_curve.csv")
    visualizeExperiment("seqtest_aamas2014.csv")
    pass

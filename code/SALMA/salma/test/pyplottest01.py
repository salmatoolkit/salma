'''
Created on 13.08.2013

@author: christian
'''
import numpy as np
import numpy.random as rnd
import matplotlib.pyplot as plt

mu, sigma = 100, 15
x = mu + sigma * rnd.randn(10000)

# the histogram of the data
n, bins, patches = plt.hist(x, 50, normed=1, facecolor='g', alpha=0.75)


plt.xlabel('Smarts')
plt.ylabel('Probability')
plt.title('Histogram of IQ')
plt.text(60, .025, r'$\mu=100,\ \sigma=15$')
plt.axis([40, 160, 0, 0.03])
plt.grid(True)

plt.show()
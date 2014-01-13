__author__ = 'kroiss'

import matplotlib.pyplot as plt
import numpy as np

x = np.arange(100)
y = x**2
#: :type : plt.Figure
fig = plt.figure()
#: :type : Axes

ax = fig.gca()
ax.set_ylim(10000, 0)
ax.set_xlim(0, 110)
# ax.get_xaxis().set_visible(False)
# ax.get_yaxis().set_visible(False)
ax.annotate("bla", xy=(100, 1000))
ax.plot(x,y)
plt.show()

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

if __name__ == '__main__':
    df = pd.read_csv("simple_robots_positions.csv", sep=";")
    fig = plt.figure()
    ax = fig.gca()
    """:type : plt.Axes"""
    ax.set_xlim(0, 220)
    ax.set_ylim(0, 150)
    ax.plot(df.x1, df.y1, label="rob1")
    ax.set_xlabel("xpos")
    ax.set_ylabel("ypos")
    ax.grid(True)

    plt.show()



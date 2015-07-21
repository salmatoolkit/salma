from matplotlib.axes._axes import Axes
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import matplotlib.patches as patches
import json
# 3 robots, 10 items
#basepath = Path("experiment_results/2015_07_20-15_53_25")


# 1 robot, 3 items
# basepath = Path("experiment_results/2015_07_20-15_47_18")

# 3 robots, 20 items
#basepath = Path("experiment_results/2015_07_21-00_29_01")
#basepath = Path("experiment_results/3r_20i_3ws-all_delivered-closest_item")
basepath = Path("experiment_results/2015_07_21-11_27_24")


def draw_locations(expsetup, ax):
    for item, desc in expsetup["items"].items():
        c = patches.Circle((desc["x"], desc["y"]), 2, color="blue")
        ax.add_patch(c)
    for item, desc in expsetup["workstations"].items():
        c = patches.Circle((desc["x"], desc["y"]), 2, color="red")
        ax.add_patch(c)


def draw_path(ax, df, robot, expsetup):
    xarr = df["x" + str(robot)].values
    yarr = df["y" + str(robot)].values
    ax.plot(xarr, yarr, antialiased=True, label=("rob" + str(robot)))
    rc = patches.Circle((xarr[0], yarr[0]), 2, color="k")
    ec = patches.Circle((xarr[len(xarr)-1], yarr[len(yarr)-1]), 4, color="k", fill=False)
    ax.add_patch(rc)
    ax.add_patch(ec)
    ax.legend(loc="lower left")


def draw_all_on_one(expsetup, df):
    fig = plt.figure()
    ax = fig.gca()
    ax.set_aspect("equal")
    """:type : plt.Axes"""
    ax.set_xlim(0, 220)
    ax.set_ylim(0, 220)
    for i in range(1, len(expsetup["robots"]) + 1):
        draw_path(ax, df, i, expsetup)

    ax.set_xlabel("xpos")
    ax.set_ylabel("ypos")
    draw_locations(expsetup, ax)
    ax.grid(True)
    fig.savefig(str(basepath.joinpath("paths_all_on_one.pdf")), bbox_inches="tight")
    plt.show()


def draw_on_subfigures(expsetup, df):
    numrobots = len(expsetup["robots"])
    fig, axes = plt.subplots(nrows=1, ncols=numrobots)
    if numrobots == 1:
        axes = [axes]
    for i, ax in enumerate(axes):
        ax.set_xlim(0, 220)
        ax.set_ylim(0, 220)
        ax.set_aspect("equal")
        draw_path(ax, df, i + 1, expsetup)

        ax.set_xlabel("xpos")
        ax.set_ylabel("ypos")
        ax.set_title("rob" + str(i + 1))
        draw_locations(expsetup, ax)

        ax.grid(True)
    fig.savefig(str(basepath.joinpath("paths_separate.pdf")), bbox_inches="tight")
    plt.show()


if __name__ == '__main__':
    with basepath.joinpath("before.json").open() as fd:
        expsetup = json.load(fd)
    df = pd.read_csv(str(basepath.joinpath("experiment.csv")), sep=";")
    draw_all_on_one(expsetup, df)
    #draw_on_subfigures(expsetup, df)

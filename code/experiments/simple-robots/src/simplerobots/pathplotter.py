from matplotlib.axes._axes import Axes
import numpy as np

import pandas as pd
import matplotlib
#matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pathlib import Path

import matplotlib.patches as patches

import json
import matplotlib.animation as manimation

# 3 robots, 10 items
# basepath = Path("experiment_results/2015_07_20-15_53_25")


# 1 robot, 3 items
# basepath = Path("experiment_results/2015_07_20-15_47_18")

# 3 robots, 20 items
# basepath = Path("experiment_results/2015_07_21-00_29_01")
# basepath = Path("experiment_results/3r_20i_3ws-all_delivered-closest_item")
# basepath = Path("experiment_results/2015_07_21-11_27_24")

# basepath = Path("experiment_results/20r_100i_10ws_500x500-collision_impossible")
#basepath = Path("experiment_results/20r_100i_10ws_500x500-no_collision")

#basepath = Path("experiment_results/2015_07_22-19_56_23")
#basepath = Path("experiment_results/2015_07_25-01_23_49")
#basepath = Path("experiment_results/3r_20i_5s_200x200_normal")
#basepath = Path("experiment_results/3r_20i_5s_200x200_clever")
basepath = Path("experiment_results/thesis_candidates/20r_100i_10ws_500x500-normal")

GRID_WIDTH = 500
GRID_HEIGHT = 500
MARGIN = 100


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
    ec = patches.Circle((xarr[len(xarr) - 1], yarr[len(yarr) - 1]), 4, color="k", fill=False)
    ax.add_patch(rc)
    ax.add_patch(ec)


def draw_all_on_one(expsetup, df):
    fig = plt.figure(figsize=(20, 20))
    ax = fig.gca()
    ax.set_aspect("equal")
    """:type : plt.Axes"""
    ax.set_xlim(0, GRID_WIDTH + MARGIN)
    ax.set_ylim(0, GRID_HEIGHT + MARGIN)
    for i in range(1, len(expsetup["robots"]) + 1):
        draw_path(ax, df, i, expsetup)

    ax.set_xlabel("xpos")
    ax.set_ylabel("ypos")
    ax.legend(loc="upper right")
    draw_locations(expsetup, ax)
    ax.grid(True)
    fig.savefig(str(basepath.joinpath("paths_all_on_one_clever.pdf")), bbox_inches="tight")
    plt.show()


def draw_on_subfigures(expsetup, df):
    numrobots = len(expsetup["robots"])
    cols = min(5, numrobots)
    rows, _ = divmod(numrobots, cols)
    fig, axes = plt.subplots(nrows=rows, ncols=cols)
    if numrobots == 1:
        axes = [axes]
    elif isinstance(axes, np.ndarray):
        axes = [a for a in axes.flatten()]
    for i, ax in enumerate(axes):
        ax.set_xlim(0, GRID_WIDTH + MARGIN)
        ax.set_ylim(0, GRID_HEIGHT + MARGIN)
        ax.set_aspect("equal")
        draw_path(ax, df, i + 1, expsetup)

        ax.set_xlabel("xpos")
        ax.set_ylabel("ypos")
        ax.set_title("rob" + str(i + 1))
        draw_locations(expsetup, ax)

        ax.grid(True)
    fig.savefig(str(basepath.joinpath("paths_separate.pdf")), bbox_inches="tight")
    plt.show()


def create_movie(expsetup, df, path):

    metadata = dict(title='Deliveryy Robots', artist='The SALMA toolkit',
                    comment='')
    print(manimation.writers.list())
    FFMpegWriter = manimation.writers['ffmpeg']
    writer = FFMpegWriter(fps=30, metadata=metadata, bitrate=2048)
    fig = plt.figure()
    ax = fig.gca()
    ax.set_aspect("equal")
    ax.set_xlim(0, GRID_WIDTH + MARGIN)
    ax.set_ylim(0, GRID_HEIGHT + MARGIN)
    draw_locations(expsetup, ax)
    lines = []
    for i in range(1, len(expsetup["robots"]) + 1):
        xarr = df["x" + str(i)].values
        yarr = df["y" + str(i)].values

        l, = ax.plot(xarr[:1], yarr[:1], antialiased=True, ls="")
        rc = patches.Circle((xarr[0], yarr[0]), 2, color="k")
        ax.add_patch(rc)
        lines.append((i, xarr, yarr, l, rc))

    #frames = len(df)
    frames = 500
    print("Frames: ", frames)
    with writer.saving(fig, path, 200):
        for steps in range(1, frames):
            for i, xarr, yarr, l, rc in lines:
                l.set_data(xarr[:steps], yarr[:steps])
                rc.center = (xarr[steps], yarr[steps])

            print("frame # ", str(steps))
            writer.grab_frame()


if __name__ == '__main__':
    with basepath.joinpath("before.json").open() as fd:
        expsetup = json.load(fd)
    df = pd.read_csv(str(basepath.joinpath("experiment.csv")), sep=";")
    draw_all_on_one(expsetup, df)
    #draw_on_subfigures(expsetup, df)
    #create_movie(expsetup, df, str(basepath.joinpath("movie.mp4")))

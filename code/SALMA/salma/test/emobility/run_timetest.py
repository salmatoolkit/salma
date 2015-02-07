from statsmodels.stats import proportion
from salma.model.experiment import SingleProcessExperimentRunner
from salma.statistics import SequentialProbabilityRatioTest
from salma.test.emobility.emobility_scenario_1 import EMobilityScenario1, ESTIMATION, VISUALIZE, HYPTEST
from salma.test.emobility.emobility_base import print_timing_info
import numpy as np
import pandas as pd
from pandas import DataFrame


def run_with_given_vehicle_number(vnum):
        sc1 = EMobilityScenario1(ESTIMATION, False, time_limit=500, num_vehicles=vnum)
        sc1.setup_properties()
        sc1.initialize()
        runner = SingleProcessExperimentRunner()
        _, results, info = runner.run_repetitions(sc1, number_of_repetitions=20)
        successes = sum(results)
        nobs = len(results)
        steps = [ti["steps"] for ti in info]

        times = [ti["time"].total_seconds() for ti in info]

        row = {"vnum": vnum, "successes": successes, "nobs": nobs,
               "time_mean": np.mean(times), "time_median": np.median(times), "time_std": np.std(times), "time_,min": np.min(times), "time_max": np.max(times),
               "step_mean": np.mean(steps), "step_median": np.median(steps), "step_std": np.std(steps), "step_,min": np.min(steps), "step_max": np.max(steps)}
        return row


if __name__ == '__main__':
    rows = []
    for i in range(1, 31):
        print("Num: {}".format(i))
        row = run_with_given_vehicle_number(i)
        print(row)
        rows.append(row)

    df = DataFrame(rows)

    df.to_csv("timetest.csv", sep=";")
    print(df)





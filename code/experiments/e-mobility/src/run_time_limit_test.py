import numpy as np
from pandas import DataFrame

from salma.model.experiment import SingleProcessExperimentRunner
from emobility_scenario_1 import EMobilityScenario1, ESTIMATION


def run_with_given_vehicle_number(tlimit):
    sc1 = EMobilityScenario1(ESTIMATION, False, time_limit=tlimit, num_vehicles=5)
    sc1.setup_properties()
    sc1.initialize()
    runner = SingleProcessExperimentRunner()
    _, results, info = runner.run_repetitions(sc1, number_of_repetitions=50)
    successes = sum(results)
    nobs = len(results)
    steps = [ti["steps"] for ti in info]

    times = [ti["time"].total_seconds() for ti in info]

    trow = {"tlimit": tlimit, "successes": successes, "nobs": nobs,
            "time_mean": np.mean(times), "time_median": np.median(times),
            "time_std": np.std(times), "time_,min": np.min(times), "time_max": np.max(times),
            "step_mean": np.mean(steps), "step_median": np.median(steps), "step_std": np.std(steps),
            "step_,min": np.min(steps), "step_max": np.max(steps)}
    return trow


if __name__ == '__main__':
    rows = []
    for i in range(5, 80, 5):
        print("Time Limit: {}".format(i))
        row = run_with_given_vehicle_number(i)
        print(row)
        rows.append(row)

    df = DataFrame(rows)

    df.to_csv("timelimit_test.csv", sep=";")
    print(df)





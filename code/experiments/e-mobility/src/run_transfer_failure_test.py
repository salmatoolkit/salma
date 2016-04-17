import numpy as np
from pandas import DataFrame

from salma.experiment import SingleProcessExperimentRunner
from emobility_scenario_1 import EMobilityScenario1, ESTIMATION
import logging
import sys


def run_with_given_transfer_end_failure(tlimit, vehicles, failure_prob):
    sc1 = EMobilityScenario1(ESTIMATION, False, time_limit=tlimit, num_vehicles=vehicles,
                             transfer_end_failure=failure_prob, transfer_starts_failure=0)
    sc1.setup_properties()
    sc1.initialize()
    runner = SingleProcessExperimentRunner()
    _, results, info = runner.run_trials(sc1, number_of_trials=50)
    successes = sum(results)
    nobs = len(results)
    steps = [ti["steps"] for ti in info]

    times = [ti["time"].total_seconds() for ti in info]

    trow = {"tlimit": tlimit, "p_failure_msg_end": failure_prob, "vehicles": vehicles, "successes": successes,
            "nobs": nobs,
            "time_mean": np.mean(times), "time_median": np.median(times),
            "time_std": np.std(times), "time_,min": np.min(times), "time_max": np.max(times),
            "step_mean": np.mean(steps), "step_median": np.median(steps), "step_std": np.std(steps),
            "step_,min": np.min(steps), "step_max": np.max(steps)}
    return trow


if __name__ == '__main__':
    logging.basicConfig()
    logger = logging.getLogger("salma")
    logger.setLevel(logging.INFO)
    rows = []
    minProb, maxProb = map(float, sys.argv[1:3]) if len(sys.argv) >= 3 else (0.1, 1.0)
    print("Increasing time limits from {} to {}".format(minProb, maxProb))
    for p_failure in np.arange(minProb, maxProb + 0.05, 0.05):
        print("Probability for failure of transferEnd: {}".format(p_failure))
        row = run_with_given_transfer_end_failure(110, 5, p_failure)
        print(row)
        rows.append(row)

    df = DataFrame(rows)

    df.to_csv("transfer_failure_test_{:.3}-to-{:.3}.csv".format(minProb, maxProb), sep=";")
    print(df)

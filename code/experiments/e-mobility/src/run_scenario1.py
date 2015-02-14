from statsmodels.stats import proportion

from salma.model.experiment import SingleProcessExperimentRunner
from salma.statistics import SequentialProbabilityRatioTest
from emobility_scenario_1 import EMobilityScenario1, ESTIMATION, VISUALIZE, HYPTEST
from emobility_base import print_timing_info


_MODE = ESTIMATION
NUM_REPETITIONS = 20

if __name__ == '__main__':
    sc1 = EMobilityScenario1(_MODE, True, time_limit=110, num_vehicles=5)
    # sc1 = EMobilityScenario1(_MODE, True, time_limit=500, num_vehicles=30)
    sc1.setup_properties()
    sc1.initialize()

    runner = SingleProcessExperimentRunner()

    # assumption success prob = 0.6 --> H0: p <= 0.4
    if _MODE == HYPTEST:
        sprt = SequentialProbabilityRatioTest(0.59, 0.61, 0.05, 0.05)
        accepted_hypothesis, results, info = runner.run_repetitions(sc1, hypothesis_test=sprt)
        print("SPRT")
        print("Conducted tests: {}".format(len(results)))
        print("Successes: {} of {}".format(sum(results), len(results)))
        print("Hypothesis accepted: {}".format(accepted_hypothesis))

        print_timing_info(info)
    elif _MODE == ESTIMATION:
        print("len: ", len(sc1.world.getDomain("plcs")))
        _, results, info = runner.run_repetitions(sc1, number_of_repetitions=NUM_REPETITIONS)

        successes = sum(results)
        nobs = len(results)
        ratio = successes / nobs
        print("ESTIMATION")
        print("Successes: {} of {}".format(successes, nobs))
        print("Ratio: {}".format(ratio))
        alpha = 0.05

        print("Confidence intervals, alpha={}: ".format(alpha))
        for method in ["normal", "agresti_coull", "beta", "wilson", "jeffrey"]:
            ci_low, ci_upp = proportion.proportion_confint(successes, nobs, alpha=alpha, method=method)
            print("   {} => [{}..{}]".format(method, ci_low, ci_upp))

        print_timing_info(info)

    elif _MODE == VISUALIZE:
        print("Visualize")
        verdict, info = sc1.run_experiment()

        print("Verdict: {}".format(verdict))
        print("Info: {}".format(info))

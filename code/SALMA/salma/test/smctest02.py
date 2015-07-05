from unittest.case import TestCase
import unittest
import logging
import logging.config
import json

from statsmodels.stats import proportion
from scipy.stats import geom

from salma.experiment import SingleProcessExperimentRunner
from salma.statistics import SequentialProbabilityRatioTest
from salma.test.smctest_base_experiment_2 import SMCTestBaseExperiment2

MODULE_LOGGER_NAME = 'salma'
logging.config.fileConfig("smctest02.logging.conf")
logger = logging.getLogger(MODULE_LOGGER_NAME)
logger.info("bla")

def report_step(world, step=None, actions=None, **kwargs):
    """
    :type world: salma.model.world.World
    """
    print("Step {}: {}".format(step, actions))
    for r in world.getDomain("robot"):
        carrying = [it.id for it in world.getDomain("item") if world.getFluentValue("carrying", [r.id, it.id])]
        print("   {}({}, {}) carries {}".format(r.id,
                                                world.getFluentValue("xpos", [r.id]),
                                                world.getFluentValue("ypos", [r.id]),
                                                carrying))


class SMCTest02(TestCase):
    """
    Conducts experiments with a simple scenario whose distribution can be given in closed form.
    """

    def setUp(self):
        logger.info("setup")
        with open("smctest02.json") as f:
            self.config = json.load(f)
            """:type : dict[str, obj]"""

        self.experiment = SMCTestBaseExperiment2(self.config)
        self.experiment.setup_properties()
        self.experiment.initialize()
        # self.experiment.step_listeners.append(report_step)

    @unittest.skip("too long")
    def test_confidence_interval_estimation(self):
        runner = SingleProcessExperimentRunner()
        sample_length = self.config["sample_length"]
        samples = self.config["samples"]
        alpha = self.config["alpha"]
        method = "agresti_coull"
        estimation_tolerance = 0.1

        confidence_intervals = []
        all_successes = 0
        report_lines = []
        """:type : list[dict]"""
        for i in range(0, samples):
            _, res, trial_infos = runner.run_trials(self.experiment,
                                                    number_of_trials=sample_length,
                                                    max_retrials=0)
            print(trial_infos)
            self.assertEqual(sample_length, len(res))
            self.assertEqual(sample_length, len(trial_infos))
            successes = sum(res)
            all_successes += successes
            ci_low, ci_up = proportion.proportion_confint(successes, len(res), alpha=alpha,
                                                          method=method)
            confidence_intervals.append((ci_low, ci_up))
            report_lines.append(dict(i=i+1, successes=successes, trials=len(res)))

            print("Run #{}: {} successes, CI: [{}..{}]".format(i + 1, successes, ci_low, ci_up))
            # self.experiment.world.printState()

        estimated_prob = all_successes / (samples * sample_length)

        real_prob = self.calc_real_prob()

        print("estimated probability: {}".format(estimated_prob))
        print("real probability: {}".format(real_prob))
        interval_hit = 0
        for cl, cu in confidence_intervals:
            if cl <= real_prob <= cu:
                interval_hit += 1
        interval_hit_ratio = interval_hit / len(confidence_intervals)
        print("interval hits: {} of {} = {} %".format(interval_hit, len(confidence_intervals),
                                                      interval_hit_ratio * 100.0))

        with open("smctest02_ci.csv", "w") as f:
            f.write("I;SUCCESSES;TRIALS\n")
            for line in report_lines:
                f.write("{i};{successes};{trials}\n".format(**line))

        self.assertAlmostEqual(real_prob, estimated_prob, delta=estimation_tolerance)
        self.assertTrue(interval_hit_ratio >= (1.0 - alpha))

    #@unittest.skip("too long")
    def test_sprt(self):
        runner = SingleProcessExperimentRunner()

        p = 0.1

        report_lines = []
        """:type : list[dict]"""

        while p <= 0.1:
            sprt = SequentialProbabilityRatioTest(p - 0.01, min(1, p + 0.01), 0.05, 0.05)
            accepted_hypothesis, res, trial_infos = runner.run_trials(self.experiment,
                                                                      hypothesis_test=sprt,
                                                                      step_listeners=[report_step],
                                                                      max_trials=200)
            print("p={:.3}".format(p))
            print("Accepted Hypothesis: {}\nResults: {}\nTrial Infos: {}".format(accepted_hypothesis, res, trial_infos))
            trials = len(trial_infos)
            print("Trials: {}".format(trials))
            report_lines.append(dict(p=p, hyp=accepted_hypothesis, trials=trials))
            p += 0.05
        with open("smctest02_sprt.csv", "w") as f:
            f.write("P;HYP;TRIALS\n")
            for line in report_lines:
                f.write("{p:.3};{hyp};{trials}\n".format(**line))

    def calc_real_prob(self):
        fail_prob_one_robot = 0
        p_dist = 1 / (self.experiment.destination_range[1] - self.experiment.destination_range[0] + 1)
        for dist in range(self.experiment.destination_range[0], self.experiment.destination_range[1] + 1):
            for (gprobspec, dprop) in zip(self.experiment.grip_probs, self.experiment.drop_probs):
                fail_prob_one_robot += p_dist * gprobspec[1] * geom.cdf(dist, dprop)
        real_prob = (1.0 - fail_prob_one_robot) ** self.experiment.num_robots
        return real_prob

    def test_calc_prob(self):
        print("real probability: {:.50}".format(self.calc_real_prob()))

    def test_visualize(self):
        pass
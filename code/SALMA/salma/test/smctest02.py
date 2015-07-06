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

    def test_confidence_interval_estimation(self):
        if "ci" not in self.config["modes"]:
            print("Skipping CI")
            return
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
        with open("smctest02_ci.csv", "w") as f:
            f.write("I;SUCCESSES;TRIALS\n")
            f.flush()
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
                line = dict(i=i+1, successes=successes, trials=len(res))
                f.write("{i};{successes};{trials}\n".format(**line))
                f.flush()
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

        self.assertAlmostEqual(real_prob, estimated_prob, delta=estimation_tolerance)
        self.assertTrue(interval_hit_ratio >= (1.0 - alpha))

    def test_sprt(self):
        if "sprt" not in self.config["modes"]:
            print("Skipping SPRT")
            return
        runner = SingleProcessExperimentRunner()

        p = self.config["sprt"]["p_min"]

        report_lines = []
        """:type : list[dict]"""
        with open("smctest02_sprt.csv", "w") as f:
            f.write("P;HYP;TRIALS\n")
            f.flush()
            while p <= self.config["sprt"]["p_max"]:
                sprt = SequentialProbabilityRatioTest(p - self.config["sprt"]["p_step"],
                                                      min(1.0, p + self.config["sprt"]["p_step"]),
                                                      self.config["sprt"]["alpha"],
                                                      self.config["sprt"]["beta"])
                accepted_hypothesis, res, trial_infos = runner.run_trials(self.experiment,
                                                                          hypothesis_test=sprt,
                                                                          max_trials=self.config["sprt"]["max_trials"])
                print("p={:.3}".format(p))
                print("Accepted Hypothesis: {}\nResults: {}\nTrial Infos: {}".format(accepted_hypothesis, res, trial_infos))
                trials = len(trial_infos)
                print("Trials: {}".format(trials))
                line = dict(p=p, hyp=accepted_hypothesis, trials=trials)
                p += self.config["sprt"]["p_step"]
                f.write("{p:.3};{hyp};{trials}\n".format(**line))
                f.flush()

    def calc_real_prob(self):
        fail_prob_one_robot = 0
        p_dist = 1 / (self.experiment.destination_range[1] - self.experiment.destination_range[0] + 1)
        for dist in range(self.experiment.destination_range[0], self.experiment.destination_range[1] + 1):
            for (gprobspec, dprop) in zip(self.experiment.grip_probs, self.experiment.drop_probs):
                fail_prob_one_robot += gprobspec[1] * geom.cdf(dist, dprop)
        fail_prob_one_robot *= p_dist
        k = 7
        print(round(fail_prob_one_robot, k))
        real_prob = (1.0 - fail_prob_one_robot) ** self.experiment.num_robots
        print(round(real_prob, k))
        rp2 = (1.0 - round(fail_prob_one_robot, k)) ** self.experiment.num_robots
        print(round(rp2, k))
        return real_prob

    def test_calc_prob(self):
        print("real probability: {:}".format(round(self.calc_real_prob(), 6)))
